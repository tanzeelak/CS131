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
        
class EchoServerClientProtocol(asyncio.Protocol):
    def __init__(self, idName, portNum):
        self.idName = idName
        self.portNum = portNum
        self.frenz = talkto[idName]
        self.clients = dict()

    async def open_server(self, message):
        asyncio.open_connection(TCP_IP, SERVER_PORTS[server],loop=loop)

    def bad_input(self, message):
        message = "? " + message
        self.transport.write(message.encode())

    def parse_location(self, latlong):
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
        parameters = {'key' : API_KEY, 'location' : str(latitude) + ',' + str(longitude), 'radius' : str(radius)}
        async with aiohttp.ClientSession() as session:
            async with session.get(GOOGLE_URL, params = parameters) as resp:
                jsonResp = (await resp.text())
                jsonObj = json.loads(jsonResp)
                jsonObj['results'] = jsonObj['results'][:int(upperBound)]
                jsonResp = json.dumps(jsonObj, indent=3)
                jsonResp += "\n\n"
                self.transport.write(jsonResp.encode())

    def handle_iamat(self, message_list):
        print(message_list)
        clientID = message_list[1]
        latlong = message_list[2]

        [latitude,longitude] = self.parse_location(latlong)
        if (float(latitude) > -180 and float(latitude) < 180) and (float(longitude) > -180 and float(longitude) < 180):
            timestamp = message_list[3]
            timeDiff = time.time() - float(timestamp)
            res = 'AT ' + self.idName + ' ' + clientID + ' ' + latitude + longitude + ' ' + str(timeDiff)
            data = res.encode(encoding='UTF-8',errors='strict')
            self.clients[clientID] = {'latitude': latitude, 'longitude': longitude, 'timestamp': timestamp}
            print(self.clients)
            self.transport.write(data)
        else:
            message  = (' ').join(message_list)
            self.bad_input(message)
        
    def handle_whatsat(self, message_list):
        print('whatsat')
        print(message_list)
        otherClientID = message_list[1]
        if otherClientID in self.clients:
            radius = message_list[2]
            upperBound = message_list[3]
            clientInfo = self.clients[otherClientID]
            timeDiff = time.time() - float(clientInfo['timestamp'])
            latitude, longitude = clientInfo['latitude'], clientInfo['longitude']
            firstMessage = "AT " + self.idName + " " + str(timeDiff) + " " + latitude + longitude + " " + clientInfo['timestamp']
            self.transport.write(firstMessage.encode())
            future = asyncio.Future()
            asyncio.ensure_future(self.query_google(future, float(latitude), float(longitude), radius, upperBound))
        else:
            message = (' ').join(message_list)
            self.bad_input(message)

    def connection_made(self, transport):
        self.transport = transport
        self.peername = transport.get_extra_info('peername')
        print('Connection from {}'.format(self.peername))
        
    def data_received(self, data):
        #check length of message
        message = data.decode()
        message_list = message.split()
        while(True): 
            if len(message_list) == 4:
                command = message_list[0]
                if command == 'IAMAT':
                    self.handle_iamat(message_list)
                    break
                elif command == 'WHATSAT':
                    self.handle_whatsat(message_list)
                    break
                else:
                    self.bad_input(message)
                    break
            else:
                self.bad_input(message)
                break

    def connection_lost(self, exc):
        print('Lost connection of {}'.format(self.peername))
        self.transport.close()

def match_serverID_port(serverID):
    if serverID in server_to_port:
        return server_to_port[serverID]
    else:
        print('u r not real')
        return sys.exit(1)
    
def main(serverID):
    portNum = match_serverID_port(serverID)
    loop = asyncio.get_event_loop()
    # Each client connection will create a new protocol instance

    # The IP address or hostname can be used.
    # 127.0.0.1 is intended for the 'localhost' loopback devices.
    # If you have multiple NIC(Network Interface Card)s, you may specify the specific IP address to be used (listen).
    # 0.0.0.0 is to use any available NIC device.
    coro = loop.create_server(lambda: EchoServerClientProtocol(serverID, portNum), '0.0.0.0', portNum)
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
        sys.stderr.write('hehe')
        exit(1)
    serverID = sys.argv[1]
    main(serverID)
