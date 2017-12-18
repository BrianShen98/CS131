import sys,time
import json
import asyncio
import functools
from iso6709 import Location
name_to_port = {
    'Alford':19530,
    'Ball':19531,
    'Hamilton':19532,
    'Holiday':19533,
    'Welsh':19534

}

propagation_rules = {
    'Alford':['Hamilton','Welsh'],
    'Ball':['Holiday','Welsh'],
    'Hamilton':['Alford','Holiday'],
    'Holiday':['Ball','Hamilton'],
    'Welsh':['Alford','Ball']
}

command_length = {
    'IAMAT':4,
    'WHATSAT':4
}

#keep track of the clients the server receive requests from
clients_list = {}

#create a protocol for server and client communication
class ClientServerProtocol(asyncio.Protocol):
    def __init__(self,name,loop):
        self.name = name
        #keep the most recent client request in the format {client:  [loc, time,server_response]}  and for serer-server communication, send the original servername with the response
        self.loop = loop
        self.fd = open(name + ".txt","a")

            
    def connection_made(self,transport):
        self.peername = transport.get_extra_info('peername')        
        self.fd.write("{}: Connected with client {}.\n".format(self.name,self.peername))
        #can used for getting extra info
        self.transport = transport
        
    def data_received(self,data):
        try:
            req = data.decode()
            req_list =req.split()
        except UnicodeDecodeError:
            self.transport.write("? \n".encode())
            self.fd.write("Connection closed by client {}.\n".format(self.peername))            
            self.fd.close()
            self.transport.close()
            return
        
        res = ""    
        if not req_list:
            self.transport.write("? \n")
            self.fd.write(self.msg_received(req))
            self.fd.write("{} responded:? . Error: Empty request.\n")
            self.fd.close()
            self.transport.close()
        
        
        elif req_list[0] == "IAMAT" and len(req_list) == command_length['IAMAT']:
            self.fd.write(self.msg_received(req))
            self.handle_IAMAT(req_list)
        elif req_list[0] == "WHATSAT" and len(req_list) == command_length['WHATSAT']:
            self.fd.write(self.msg_received(req))
            self.handle_WHATSAT(req_list)
        elif req_list[0] == "PROP":
            self.fd.write("{} received flooded message:{}.\n".format(self.name,req))
            self.handle_PROP(req)
        else:
            res = "? "+ req
            self.transport.write(res.encode())
            self.fd.write(self.msg_received(req))
            self.fd.write("{} responded:{}. Error: Invalid name of the command.\n".format(self.name,res))
            self.fd.close()
            self.transport.close()


    def msg_received(self,msg):
        return "{} received:{}.\n".format(self.name,msg)

    def handle_IAMAT(self,message):
        # send PROP to other server
        res = ""
        clientID = message[1]
        loc = message[2]
        req_time = message[3]
        res_time = time.time()
        
        #check the format of the location
        if not check_loc(loc):
            res = err_msg(message)
            self.fd.write("{} responded:{}. Error: The location is not in ISO 6709 fromat.\n".format(self.name,res))
            self.fd.close()
        #check the format of POSIX time
        elif not check_time(req_time):
            res = err_msg(message)
            self.fd.write("{} responded:{}. Error: The timestamp is not in POSIX time fromat.\n".format(self.name,res))
            self.fd.close()
        else:        
            time_diff = str(res_time - float(req_time))
            if '-' not in  time_diff:
                time_diff = '+' + time_diff
            res = 'AT '+self.name+' ' + time_diff + ' ' + ' '.join(message[1:])+'\n'
            self.fd.write("{} responded:{}.\n".format(self.name,res))
            #flood the newest info to other servers
            asyncio.ensure_future(self.flooding("PROP "+res+" "+req_time),loop = self.loop)
            #update communication records
            lag_longi = get_loc(loc)
            lag_longi.extend([req_time,res])
            if clientID in clients_list:
                old_time = clients_list[clientID][2]
                if float(req_time) > float(old_time):
                    clients_list[clientID] = lag_longi
                   
            else:
                clients_list[clientID] = lag_longi
            
        self.transport.write(res.encode())        
        self.transport.close()
        
    def handle_WHATSAT(self,message):    
        clientID = message[1]
        res = ""
        try:
            radius = float(message[2])
            infobd = int(message[3])
        except ValueError:
            res = err_msg(message)
            self.fd.write("{} responded:{}. Error: Cannot recognize radius or information bound.\n".format(self.name,res))
            self.fd.close()
            self.transport.write(res.encode())
            self.transport.close()
            return
        

        #error check
        if radius > 50 or radius < 0:
            res =  err_msg(message)
            self.fd.write("{} responded:{}. Error: Radius out of bound.\n".format(self.name,res))
            self.fd.close()
            self.transport.write(res.encode())
            self.transport.close()
        elif infobd > 20 or infobd < 0:            
            res =  err_msg(message)
            self.fd.write("{} responded:{}. Error: Information bound out of bound.\n".format(self.name,res))
            self.fd.close()
            self.transport.write(res.encode())
            self.transport.close()
        else:            
            #ask Google place to parse 
            if clientID in clients_list:
                #since the loop is already running,it will start the func until await
                task = asyncio.ensure_future(self.req_google(clients_list[clientID][0],clients_list[clientID][1],radius),loop = self.loop)
            
                task.add_done_callback(functools.partial(self.process_json,clientID,infobd))
            else:
                #the client being queried does not exist
                res = err_msg(message)
                self.fd.write("{} responded:{}. Error: The client asked for does not exist.\n".format(self.name,res))
                self.fd.close()
                self.transport.write(res.encode())
                self.transport.close()
                

    #format of PROP: "PROP ORIGINS RESPONSE REQ_TIME"
    async def flooding(self,proped_msg):
        root = ""
        #parse proped_msg to find origins
        try:
            AT_idx = proped_msg.index("AT")
            origins = proped_msg[5:AT_idx].split()
            if not origins:
                root = self.name+" "
        except ValueError:        
            return
        
        flood_list = [target for target in propagation_rules[self.name] if target not in origins]
        #form new PROP msg
        msg = proped_msg[:AT_idx] + root + ' '.join(flood_list)+" "+proped_msg[AT_idx:]
        
        #flood to targets
        for target in flood_list:
            try:
                self.fd.write("{}:try to flood to {}.\n".format(self.name,target))
                connection = asyncio.open_connection('127.0.0.1',name_to_port[target],loop = self.loop)
                (reader,writer) = await connection        
                writer.write(msg.encode())
                await writer.drain()            
                writer.close()

            except ConnectionRefusedError:
                self.fd.write("{}:{} is offline.\n".format(self.name,target))            
                continue
            else:
                self.fd.write("{}:flooded to {}.\n".format(self.name,target))
        self.fd.close()

        
    def handle_PROP(self,req):
        #parse
        try:
            AT_idx = req.index("AT")
            time_idx = req.rindex(" ")
            msg = req[AT_idx:time_idx]
            msg_list = msg.split()
            req_time = req[time_idx+1:]            
            client = msg_list[3]
            lag_longi = get_loc(msg_list[4])
            lag_longi.extend([req_time,msg])
            
        except ValueError:
            return

        #update the clients info
        if client in clients_list:
            old_time = clients_list[client][2]
            if(float(req_time) >= float(old_time)):
                clients_list[client] = lag_longi
        else:
            clients_list[client] = lag_longi

        #flood to others
        asyncio.ensure_future(self.flooding(req),loop = self.loop)
        

    async def req_google(self,lag,longi,radius):
        api_key = "AIzaSyCqDjGGjA4UxSHo_GXa_MUdVfvTPkdJqDc"
        host = "maps.googleapis.com"
        uri = "/maps/api/place/nearbysearch/json?location={}&radius={}&key={}".format(lag+","+longi,radius*1000,api_key)
        try:
            connection = asyncio.open_connection(host,443,ssl=True,loop = self.loop)
            (reader,writer) = await connection
            self.fd.write("{}:Try to connect to Google.\n".format(self.name))
        except ConnectionRefusedError:
            self.fd.write("{}:Failed to connect to Google.\n".format(self.name))
            return
        else:
            req = "GET {} HTTP/1.1\r\nHost: {}\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n\r\n".format(uri,host)
            self.fd.write("{}:Connection established. Send request:{}".format(self.name,req))
            writer.write(req.encode())
            await writer.drain()
            #discard the header
            head = await reader.readuntil("\r\n\r\n".encode())
            body = await reader.readuntil("\r\n\r\n".encode())
            writer.close()
        
            return body.decode()
        

    def process_json(self,clientID,infobd,future):
        body = future.result()    
        if not body:
            self.transport.close()
            self.fd.close()
            return
        
        places = body[body.index('{'):body.rindex('}')+1]

        #eliminate adjacent new lines
        temp1 = places;
        temp2 = temp1.replace('\n\n','\n')
        while temp1 != temp2:
            temp1 = temp2
            temp2 = temp1.replace('\n\n','\n')
        places = temp2

        #select limited results
        
        obj = json.loads(places,strict = False)
        obj['results'] = obj['results'] if len(obj['results']) <= infobd else obj['results'][:infobd]
        
        #form response
        res = clients_list[clientID][3] + json.dumps(obj,indent = 3,separators=(',',': '))+'\n\n'
        self.fd.write("{} responded:{}".format(self.name,clients_list[clientID][3]))
        self.fd.close()
        self.transport.write(res.encode())
        self.transport.close()

        
def err_msg(msg):
        return '? '+ ' '.join(msg)+'\n'
    
#check the format of ISO6079 location
def check_loc(loc):
    try:
        laglongi = Location(loc)
    except AttributeError:
        return False

    return True
    # if loc.count("+") + loc.count("-") != 2:
    #     return False
    # lag_longi = loc.replace("+"," +").replace("-"," -").split()    
    # try:
    #     lag = float(lag_longi[0])
    #     longi = float(lag_longi[1])
    # except ValueError:
    #     return False
    # return True

def get_loc(loc):
    location = Location(loc)
    return [str(location.lat.degrees), str(location.lng.degrees)]

#check POSIX time            
def check_time(time):
    try:
        if float(time) < 0:
            return False
    except ValueError:
        return False
    return True
            
        
def main():
    if(len(sys.argv) != 2):
        print("Please privide a valid server name!")
        
    else:
        server_name = sys.argv[1]
        loop = asyncio.get_event_loop()
        #since create_server() is a coroutine function, this will return a coroutine object
        try:
            coro = loop.create_server(lambda:ClientServerProtocol(server_name,loop),'127.0.0.1',name_to_port[server_name])
        except KeyError:
            print("The server name does not exist!")
            return
        
        #may be add the coroutine object into the loop and initialize it
        server = loop.run_until_complete(coro)
        
        try:
            loop.run_forever()
        except KeyboardInterrupt:
            pass
        
        server.close()
        loop.run_until_complete(server.wait_closed())
        loop.close()

if __name__ == "__main__":
    main()
    
