from cryptography.fernet import Fernet

key = 'TluxwB3fV_GWuLkR1_BzGs1Zk90TYAuhNMZP_0q4WyM='

# Oh no! The code is going over the edge! What are you going to do?
message = b'gAAAAABc6V3oeScWIGDzWdULY_Z-0lMQfZtSiBlRiTL9E1JMkQu9Zc6BFq6MiBr-wIiAl7XUxIi6eyFrTpx4w_XaEeCpTzd64HHtU85l_2BlV0NdIrY-OZGFnlFnmmkwiow1t-bXoKZgxp9_brWxbhvzqiGfe5fP52RGi0kwWAh7luPVqOVkCIk='

def main():
    f = Fernet(key)
    print(f.decrypt(message))


if __name__ == "__main__":
    main()

