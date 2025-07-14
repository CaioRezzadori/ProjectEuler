def encode(message: str, rails: int) -> str:
    """
    Encodes message with rail fence cipher
    """
    if rails == 1 or rails >= len(message):
        return message
    encoded_message = ""
    message_size = len(message)
    for row in range(rails):
        if row in (0, rails - 1):  # Rail extremes
            step = 2 * (rails - 1)
            encoded_message += "".join(
                [message[j] for j in range(row, message_size, step)]
            )
        else:  # Intermediate rails
            step_down = 2 * (rails - row - 1)
            step_up = 2 * row
            direction_down = True
            current_pos = row

            while current_pos < message_size:
                encoded_message += message[current_pos]
                if direction_down:
                    current_pos += step_down
                else:
                    current_pos += step_up
                direction_down = not direction_down
    return encoded_message


def row_chunks__(encoded_message: str, rails: int) -> list[list[str]]:
    """
    Separete encoded message to rows chunks following rail fence cipher
    """
    chunks: list[list[str]] = [[] for _ in range(rails)]
    message_size = len(encoded_message)
    last_id = 0
    for row in range(rails):
        if row == 0:
            step = 2 * (rails - 1)
            last_id = (message_size - 1) // step + 1
            chunks[row].extend(encoded_message[row:last_id])
        else:
            step_down = 2 * (rails - row - 1)
            step_up = 2 * row
            direction_down = True
            current_pos = row
            count = last_id
            while current_pos < message_size:
                count += 1
                if direction_down:
                    current_pos += step_down
                else:
                    current_pos += step_up
                direction_down = not direction_down
            chunks[row].extend(encoded_message[last_id:count])
            last_id = count
    return chunks


def decode(encoded_message: str, rails: int) -> str:
    """
    Decodes rail fence cipher
    """
    if rails == 1 or rails >= len(encoded_message):
        return encoded_message
    chunks = row_chunks__(encoded_message, rails)
    message = ""
    direction = -1
    i = 0
    while any([len(x) > 0 for x in chunks]):
        message += chunks[i][0]
        del chunks[i][0]
        if i in (0, rails - 1):
            direction *= -1
        i += direction
    return message
