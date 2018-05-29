import asyncio

class EchoServerClientProtocol(asyncio.Protocol):
    def connection_made(self, transport):
        self.transport = transport
        self.peername = transport.get_extra_info('peername')
        
        print('Connection from {}'.format(self.peername))

    def data_received(self, data):
        message = data.decode()
        print('Data received: {!r}'.format(message))

        print('Send: {!r}'.format(message))
        self.transport.write(data)

    def connection_lost(self, exc):
        print('Lost connection of {}'.format(self.peername))
        self.transport.close()

loop = asyncio.get_event_loop()
# Each client connection will create a new protocol instance

# The IP address or hostname can be used.
# 127.0.0.1 is intended for the 'localhost' loopback devices.
# If you have multiple NIC(Network Interface Card)s, you may specify the specific IP address to be used (listen).
# 0.0.0.0 is to use any available NIC device.

coro = loop.create_server(EchoServerClientProtocol, '0.0.0.0', 8888)
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