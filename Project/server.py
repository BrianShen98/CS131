import asyncio
import re
import string
import sys
import time
import json
import logging

repo = {}

def name_to_port(name):
	return{
		'Alford': 19530,
		'Hamilton': 19531,
		'Holiday': 19532,
		'Ball': 19533,
		'Welsh': 19534
	}.get(name, -1)

def socketMatch(socket):
	return{
		19530: 'Alford',
		19531: 'Hamilton',
		19532: 'Holiday',
		19533: 'Ball',
		19534: 'Welsh'
	}.get(socket)

def propagation_rules(name):
	return{
		'Alford': (19534, 19531),
		'Hamilton': (19530, 19532),
		'Holiday': (19531, 19533),
		'Ball': (19532, 19534),
		'Welsh': (19533, 19530)
	}.get(name)

def check_IAMAT(s):
	regex = re.compile(r"""IAMAT\n
							(.*)\n
							[+-]\d+\.\d+[+-]\d+\.\d+\n
							\d+\.\d{9}""", re.VERBOSE)
	result = regex.fullmatch(s)
	if(not result):
		return False
	for chr in result.group(1):
		if chr in string.whitespace:
			return False
	return True

def check_WHATSAT(s):
	regex = re.compile(r"""WHATSAT\n
							(.*)\n
							(\d*)\n
							(\d*)""", re.VERBOSE)
	result = regex.fullmatch(s)
	if(not result):
		return False
	for chr in result.group(1):
		if chr in string.whitespace:
			return False
	if int(result.group(2)) > 50 or int(result.group(3)) > 20:
		return False
	return True

def check_TCPMessage(msg):
	l = msg.split()
	length = len(l)
	if length == 6 and l[0] == 'AT':
		return True
	elif length == 4:
		if l[0] == 'IAMAT':
			return check_IAMAT('\n'.join(msg.split()))
		elif l[0] == 'WHATSAT':
			return check_WHATSAT('\n'.join(msg.split()))
		return False
	return False

class ServerClientProtocol(asyncio.Protocol):
	def __init__(self, message, loop): # message in case of client-like
		self.message = message # None for server-like
		self.loop = loop
		
	def connection_made(self, transport):
		if not self.message: # server
			self.transport = transport
			self.socket = transport.get_extra_info('sockname')[1]
			self.name = socketMatch(self.socket)
			peername = transport.get_extra_info('peername')
			logging.info('{}: connection from {}'.format(self.name, peername))
		else: # client
			self.transport = transport
			transport.write(self.message.encode())

	def _IAMATProcess(self, keywords):
		id = keywords[1]
		req_time = float(keywords[3])
		repoValue = repo.get(id) # get a tuple
		if  repoValue == None or repoValue[0] <= req_time:
			timediff = time.time() - req_time
			timediffstr = format(timediff, '.9f')
			timestamp = '+'+timediffstr if timediff >= 0 else timediffstr
			ATstr = 'AT ' + self.name + ' ' + timestamp + ' ' + \
						' '.join([keywords[i] for i in range(1, 4)]) + '\r\n'
			repo[id] = (req_time, ATstr)
			return ATstr
		return repoValue[1]

	async def flooding(self, ATString):
		for socket in propagation_rules(self.name):
			try:
				coro = self.loop.create_connection(lambda: ServerClientProtocol(ATString, self.loop), '127.0.0.1', socket)
				await self.loop.create_task(coro)
			except ConnectionRefusedError as e:
				logging.info('{} flooding stopped: to {}, {}'.format(self.name, socketMatch(socket), e))
			else:
				logging.info('{} floods: to {}'.format(self.name, socketMatch(socket)))

	def getProcess(self, radius, geo):
		radius *= 1000 #kilos to meters
		regex = re.compile(r'([+-]\d+\.\d+)([+-]\d+\.\d+)')
		result = regex.match(geo)
		geo1 = result.group(1)
		geo2 = result.group(2)
		uri = \
			'/maps/api/place/nearbysearch/json?location='+geo1+','+geo2+\
			'&radius='+str(radius)+\
			'&key=AIzaSyCqDjGGjA4UxSHo_GXa_MUdVfvTPkdJqDc'
		getRequest = \
			'GET ' + uri + ' HTTP/1.1\r\n' +\
			'Host: ' + 'maps.googleapis.com\r\n' +\
			'Content-Type: text/plain; charset=utf-8\r\n' +\
			'Connection: close\r\n\r\n'
		logging.info('getRequest:\n{}'.format(getRequest))
		return getRequest

	async def googleAPI(self, getRequest, bound):
		reader, writer = await asyncio.open_connection('maps.googleapis.com', 443, ssl=True, loop=self.loop)
		writer.write(getRequest.encode())
		await writer.drain()

		data = await reader.read()
		message = data.decode()
		getjson = json.loads(message[message.find("{"):])
		getjson['results'] = len(getjson['results']) <= bound and getjson['results'] or getjson['results'][:bound]
		jsonOutput = json.dumps(getjson, indent=4, sort_keys=True, separators=(',', ': ')) + '\n\n'
		
		writer.close()

		self.transport.write(jsonOutput.encode())
		logging.info('{}: close the client socket'.format(self.name))
		self.transport.close()

	def _errorHandler(self, message):
		errorMessage = '? '+message
		logging.info('{} reported error: {!r}'.format(self.name, errorMessage))
		self.transport.write(errorMessage.encode())

	def data_received(self, data):
		try:
			message = data.decode() ## TODO: special char cannot be decoded like \c
		except UnicodeDecodeError as e:
			self.transport.write('? '.encode())
			self.transport.write(data)
			logging.info('{}: cannot debug received data by utf-8'.format(self.name))
			return
		logging.info('{} received: {!r}'.format(self.name, message))
		if check_TCPMessage(message):
			keywords = message.split()
			command = keywords[0]
			if command == 'IAMAT':
				# respond with AT message
				ATString = self._IAMATProcess(keywords)
				logging.info('{} responded: {!r}'.format(self.name, ATString))
				self.transport.write(ATString.encode())

				# spread message to herd
				self.loop.create_task(self.flooding(ATString))
				logging.info('{}: close the client socket'.format(self.name))
				self.transport.close()

			elif command == 'WHATSAT':
				id = keywords[1]
				repoValue = repo.get(id)
				if not repoValue:
					# id not found
					self._errorHandler(message)
					logging.info('{}: close the client socket'.format(self.name))
					self.transport.close()
				else:
					# respond with AT message after found
					ATString = repoValue[1]
					logging.info('{} responded: {!r}'.format(self.name, ATString))
					self.transport.write(ATString.encode())
					getRequest = self.getProcess(int(keywords[2]), ATString.split()[4])
					self.loop.create_task(self.googleAPI(getRequest, int(keywords[3])))

			else: # get AT message
				id = keywords[3]
				repoValue = repo.get(id)
				req_time = float(keywords[5])
				if not repoValue or (req_time > repoValue[0]):
					logging.info('repotime: {}, messagetime: {}' .format(-1 if not repoValue else repoValue[0], req_time))
					repo[id] = (req_time, message)
					self.loop.create_task(self.flooding(message))
				logging.info('{}: close the client socket'.format(self.name))
				self.transport.close()
		
		else:
			# wrong format message
			self._errorHandler(message)
			logging.info('{}: close the client socket'.format(self.name))
			self.transport.close()
		
	def connection_lost(self, _):
		if self.message: # a client, whose message is not None
			self.transport.close()

def main():
	if len(sys.argv) != 2:
		print ('Invalid number of paramaters: Only 1 arg')
		return
	name = sys.argv[1]
	socket = name_to_port(name)
	if socket == -1:
		print ('Invalid server name')
		return
	
	logging.basicConfig(filename='{}.log'.format(name), level=logging.INFO)
	
	loop = asyncio.get_event_loop()
	loop.set_debug(True)
	# Each client connection will create a new protocol instance
	coro = loop.create_server(lambda: ServerClientProtocol(None, loop), '127.0.0.1', socket)
	server = loop.run_until_complete(coro)

	# Serve requests until Ctrl+C is pressed
	logging.info('{}: serving on {}'.format(name, socket))
	try:
		loop.run_forever()
	except KeyboardInterrupt:
		pass

	# Close the server
	server.close()
	loop.run_until_complete(server.wait_closed())
	loop.close()

if __name__ == '__main__':
	main()
