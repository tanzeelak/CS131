import asyncio
import aiohttp
import sys
import logging
import time

API_KEY =  'AIzaSyAq2jI8Y0mp7475Fe_0Bu3Q2nxW5gkQeB4'
GOOGLE_URL = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?'
talkto = {
    'Goloman': ['Hands', 'Holiday', 'Wilkes'],
    'Hands': ['Goloman', 'Wilkes'],
    'Holiday': ['Goloman', 'Welsh', 'Wilkes'],
    'Wilkes': ['Goloman', 'Hands', 'Holiday'],
    'Welsh': ['Holiday']
}
        
class EchoServerClientProtocol(asyncio.Protocol):
    def __init__(self, idName, portNum):
        self.idName = idName
        self.portNum = portNum
        self.frenz = talkto[idName]

    async def fetch(self, session, url):
        async with session.get(url) as response:
            return await response.text()

    async def query_google(self, future):
        #url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=-33.8670522,151.1957362&radius=1500&type=restaurant&keyword=cruise&key=AIzaSyAq2jI8Y0mp7475Fe_0Bu3Q2nxW5gkQeB4'
        latitude = -33.8670522
        longitude = 151.1957362
        radius = 1500
        parameters = {'key' : API_KEY, 'location' : str(latitude) + ',' + str(longitude), 'radius' : str(radius)}
        async with aiohttp.ClientSession() as session:
            async with session.get(GOOGLE_URL, params = parameters) as resp:
                res = (await resp.text())
                self.transport.write(res.encode())

    def handle_iamat(self, message_list):
        print(message_list)
        clientID = message_list[1]
        latlong = message_list[2] #check if number is between -180 to 180
        timestamp = message_list[3]
        timeDiff = time.time() - float(timestamp)
        res = 'AT ' + self.idName + ' ' + clientID + ' ' + latlong + ' ' + str(timeDiff)
        data = res.encode(encoding='UTF-8',errors='strict')
        self.transport.write(data)
        
    def handle_whatsat(self, message_list):
        print('whatsat')
        print(message_list)
        otherClientID = message_list[1]
        radius = message_list[2]
        upperBound = message_list[3]
        timestamp = message_list[3]
        timeDiff = time.time() - float(timestamp)
        res = 'AT ' + self.idName + ' ' + otherClientID + ' 345678 ' + str(timeDiff)
        data = res.encode(encoding='UTF-8',errors='strict')
        future = asyncio.Future()
        asyncio.ensure_future(self.query_google(future))
        
    def connection_made(self, transport):
        self.transport = transport
        self.peername = transport.get_extra_info('peername')
        print('Connection from {}'.format(self.peername))
        
    def data_received(self, data):
        #check length of message
        message = data.decode()
        message_list = message.split()
        if len(message_list) is not 0:
            command = message_list[0]
            if command == 'IAMAT':
                self.handle_iamat(message_list)
            elif command == 'WHATSAT':
                self.handle_whatsat(message_list)
            else:
                print('garbage')

    def connection_lost(self, exc):
        print('Lost connection of {}'.format(self.peername))
        self.transport.close()

def match_serverID_port(serverID):
    if serverID == 'Goloman':
        return 12285
    elif serverID == 'Hands':
        return 12286
    elif serverID == 'Holiday':
        return 12287
    elif serverID == 'Wilkes':
        return 12288
    elif serverID == 'Welsh':
        return 12289
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
