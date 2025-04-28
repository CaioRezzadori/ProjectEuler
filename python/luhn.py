def luhnCheck(n):
    if(len(n) <= 1): return(False)
    n = n.replace(" ", "") # Removing spaces
    digits = [int(x) for x in n]
    indexSelect = list(range(0, len(digits), 2))

    digits = [x*2 if x.index % 2 == 0 else x for x in digits ]
    digits = [x - 9 if x > 9 else xfor x in digits if x > 9]
    print(digits)
    return(sum(digits))

teste = [1,2, 3, 4]
teste[0:len(teste):2] = teste[0:len(teste):2]*

luhnCheck("4539 3195 0343 6467")

teste = "4539 3195 0343 6467"
list[teste.replace(" ", "")]


