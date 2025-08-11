def kangaroo(kanga1: float, speed1: float, kanga2: float, speed2: float) -> bool:
    """
    Returns boolean if the two kangaroos meet after a number of jumps, given]
    their speeds
    """
    delta_distance = kanga2 - kanga1
    relative_speed = speed1 - speed2

    if relative_speed * delta_distance > 0:
        delta_distance = abs(delta_distance)
        relative_speed = abs(relative_speed)
        return delta_distance % relative_speed == 0
    return False
