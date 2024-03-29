import asyncio
import aiohttp
import sys
import logging
import time
import json

API_KEY =  'AIzaSyAq2jI8Y0mp7475Fe_0Bu3Q2nxW5gkQeB4'
GOOGLE_URL = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?'
talkto = {
    'Goloman': ['Hands', 'Holiday', 'Wilkes'],
    'Hands': ['Goloman', 'Wilkes'],
    'Holiday': ['Goloman', 'Welsh', 'Wilkes'],
    'Wilkes': ['Goloman', 'Hands', 'Holiday'],
    'Welsh': ['Holiday']
}
server_to_port = {
    'Goloman': 12285,
    'Hands': 12286,
    'Holiday': 12287,
    'Wilkes': 12288,
    'Welsh': 12289
}

clients = dict()
        
class EchoServerClientProtocol(asyncio.Protocol):
    def __init__(self, idName, portNum, loop):
        self.idName = idName
        self.portNum = portNum
        self.loop = loop
        self.frenz = talkto[idName]

    async def flooding(self, future, message):
        for friend in self.frenz:
            try: 
                #print("FLOOD from {} to {}: {}".format(self.idName, friend, message))
                reader, writer = await asyncio.open_connection('127.0.0.1', server_to_port[friend])
                writer.write(message.encode())
                await writer.drain()
                writer.close()
            except ConnectionRefusedError:
                logging.info('ConnectionRefusedError. {} cannot connect to {}'.format(self.idName, friend))

    def bad_input(self, message):
        message = "? " + message
        logging.info(message)
        self.transport.write(message.encode())

    def parse_coord(self, latlong):
        latitude = ''
        longitude = ''
        latFlag = False
        for c in latlong:
            if latFlag == False and (c == '+' or c == '-'):
                latitude += c
                latFlag = True
            elif latFlag == True  and (c == '+' or c == '-'):
                longitude += c
                latFlag = False
            elif latFlag == True:
                latitude += c
            elif latFlag == False:
                longitude += c
        return(latitude,longitude)
        
    async def query_google(self, future, latitude, longitude, radius, upperBound):
        vals = {'key' : API_KEY, 'location' : str(latitude) + ',' + str(longitude), 'radius' : str(radius)}
        async with aiohttp.ClientSession() as session:
            async with session.get(GOOGLE_URL, params = vals) as resp:
                jsonResp = (await resp.text())
                jsonObj = json.loads(jsonResp)
                jsonObj['results'] = jsonObj['results'][:int(upperBound)]
                jsonResp = json.dumps(jsonObj, indent=3)
                jsonResp += "\n\n"
                logging.info(jsonResp)
                self.transport.write(jsonResp.encode())

    def handle_iamat(self, message_list):
        #print('iamat')
        #print(message_list)
        message = ' '.join(message_list)
        clientID = message_list[1]
        latlong = message_list[2]
        [latitude,longitude] = self.parse_coord(latlong)
        timestamp = message_list[3]
        timeDiff = time.time() - float(timestamp)
        propMessage = 'AT ' + self.idName + ' ' + str(timeDiff) + ' ' + clientID + ' ' + latitude + longitude + ' ' + timestamp
        logging.info(propMessage)
        self.transport.write(propMessage.encode())        
        if (clientID not in clients) or ((clientID in clients) and (float(clients[clientID]['timestamp']) < float(timestamp))):
            clients[clientID] = {'latitude': latitude, 'longitude': longitude, 'timestamp': timestamp, 'timeDiff': str(timeDiff), 'origServer': self.idName}
            future = asyncio.Future()
            asyncio.ensure_future(self.flooding(future, propMessage))
        
    def handle_whatsat(self, message_list):
        #print('whatsat')
        #print(message_list)
        otherClientID = message_list[1]
        if otherClientID in clients:
            radius = message_list[2]
            upperBound = message_list[3]
            clientInfo = clients[otherClientID]
            latitude, longitude = clientInfo['latitude'], clientInfo['longitude']
            firstMessage = 'AT ' + clientInfo['origServer'] + ' ' + clientInfo['timeDiff'] + ' ' + otherClientID + ' '  + latitude + longitude + ' ' + clientInfo['timestamp']
            logging.info(firstMessage)
            self.transport.write(firstMessage.encode())
            future = asyncio.Future()
            asyncio.ensure_future(self.query_google(future, float(latitude), float(longitude), radius, upperBound))
        else:
            message = ' '.join(message_list)
            self.bad_input(message)

    def handle_at(self, message_list):
        message = ' '.join(message_list)
        origServer = message_list[1]
        timeDiff = message_list[2]
        clientID = message_list[3]
        latlong = message_list[4]
        [latitude,longitude] = self.parse_coord(latlong)
        timestamp = message_list[5]
        if (clientID not in clients) or ((clientID in clients) and (float(clients[clientID]['timestamp']) < float(timestamp))):
            clients[clientID] = {'latitude': latitude, 'longitude': longitude, 'timestamp': timestamp, 'timeDiff': timeDiff, 'origServer': origServer}
            future = asyncio.Future()
            asyncio.ensure_future(self.flooding(future, message))

    def connection_made(self, transport):
        #log new connection
        self.transport = transport
        self.peername = transport.get_extra_info('peername')
        logInput = 'Connection from {}'.format(self.peername)
        print(logInput)
        logging.info(logInput)
        
    def data_received(self, data):
        # input and output logged
        message = data.decode()
        logging.info(message)
        message_list = message.split()
        if len(message_list) == 4:
            command = message_list[0]
            if command == 'IAMAT':
                self.handle_iamat(message_list)
            elif command == 'WHATSAT':
                self.handle_whatsat(message_list)
            else:
                self.bad_input(message)
        elif len(message_list) == 6:
            command = message_list[0]
            if command == 'AT':
                self.handle_at(message_list)
        else:
            self.bad_input(message)

    def connection_lost(self, exc):
        #log lost connection
        logInput = 'Lost connection of {}'.format(self.peername)
        #print(logInput)
        logging.info(logInput)
        self.transport.close()

def match_serverID_port(serverID):
    if serverID in server_to_port:
        return server_to_port[serverID]
    else:
        print('Invalid server')
        return sys.exit(1)
    
def main(serverID):
    logging.basicConfig(filename='logfile.log',level=logging.INFO)
    portNum = match_serverID_port(serverID)
    loop = asyncio.get_event_loop()
    # Each client connection will create a new protocol instance

    # The IP address or hostname can be used.
    # 127.0.0.1 is intended for the 'localhost' loopback devices.
    # If you have multiple NIC(Network Interface Card)s, you may specify the specific IP address to be used (listen).
    # 0.0.0.0 is to use any available NIC device.
    coro = loop.create_server(lambda: EchoServerClientProtocol(serverID, portNum, loop), '0.0.0.0', portNum)
    server = loop.run_until_complete(coro)

    # Serve requests until Ctrl+C is pressed
    print('Serving on {}'.format(server.sockets[0].getsockname()))
    try:
        loop.run_forever()
    except KeyboardInterrupt:
        pass

    # Close the server
    server.close()
    loop.run_until_complete(server.wait_closed())
    loop.close()

if __name__ == '__main__':
    if (len(sys.argv) != 2):
        sys.stderr.write('Not enuf args plz')
        exit(1)
    serverID = sys.argv[1]
    main(serverID)
