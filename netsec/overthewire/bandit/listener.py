import socket
import sys

# create a TCP/IP socket
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

#bind the socket to the port
server_address = ('localhost', 10000)
sys.stderr.write(f'starting up on {server_address[0]} port {server_address[1]}')
sock.bind(server_address)

sock.listen(1)

while True:
    # Wait for a connection
    sys.stderr.write(f'waiting for a connection')
    connection, client_address = sock.accept()

    with connection:
        print(f'Connection with f{client_address} established')
        # while True:
        data = b'GbKksEFF4yrVs6il55v6gwY5aVje5f0j'
        print('Sending data')
        connection.send(data)
        received_data = connection.recv(1024)
        if received_data:
            print(received_data)
