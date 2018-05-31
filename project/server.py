import asyncio
import aiohttp
import sys
import logging
import argparse

class EchoServerClientProtocol(asyncio.Protocol):
    def __init__(self, idNum, portNum):
        self.idNum = idNum
        self.portNum = portNum

    def connection_made(self, transport):
        self.transport = transport
        self.peername = transport.get_extra_info('peername')
        
        print('Connection from {}'.format(self.peername))

    def data_received(self, data):
        #check length of message
        message = data.decode()
        message_list = message.split()
        command = message_list[0]
        clientID = message_list[1]
        latitude = message_list[2]
        longitude = message_list[3]
        print(command)
        print(clientID)
        print(latitude) #check if number is between -180 to 180
        print(longitude)

        print('Data received: {!r}'.format(message))

        print('Send: {!r}'.format(message))
        self.transport.write(data)

    def connection_lost(self, exc):
        print('Lost connection of {}'.format(self.peername))
        self.transport.close()


def match_serverID_port(serverID):
    return 8888
    
def main(serverID):
    portNum = match_serverID_port(serverID)

    talkto = {
        "Goloman": ["Hands", "Holiday", "Wilkes"],
        "Hands": ["Goloman", "Wilkes"],
        "Holiday": ["Goloman", "Welsh", "Wilkes"],
        "Wilkes": ["Goloman", "Hands", "Holiday"],
        "Welsh": ["Holiday"]
    }
    
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

if __name__ == "__main__":
    if (len(sys.argv) != 2):
        sys.stderr.write("hehe")
        exit(1)
    serverID = sys.argv[1]
    
    main(serverID)
