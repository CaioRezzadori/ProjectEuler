def encode(message: str, rails: int) -> str:
    if rails == 1:
         return message
    encodedMessage = ''
    for row in range(rails):
        if row == 0 or row == rails - 1:
             for j in range(row, len(message), 2 + 2*(rails - 2)):
                encodedMessage += message[j]
        else:
            neigh1 = 2 + 2*(rails - row - 2)
            neigh2 = 2*row
            id = row
            count = 1
            while id < len(message):
                    encodedMessage += message[id]
                    id += neigh1*(count % 2) + neigh2*((count + 1) % 2)
                    count += 1  
    return encodedMessage

def decode(encoded_message: str, rails: int) -> str:
    if rails == 1:
         return encoded_message
    message = ' '*len(encoded_message)
    for row in range(rails):
         