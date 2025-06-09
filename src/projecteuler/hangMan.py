def printHangman(nTries: int) -> None:
  '''
  Prints hangman state depending on number of tries.
  '''
  match nTries:
    case 0:
      print(' ----------\n'
            ' |        |\n',
            '|\n',
            '|\n' ,
            '|\n',
            '|\n',
            '----------\n')
    case 1:
      print(' ----------\n'
            ' |        |\n',
            '|        O\n',
            '|\n',
            '|\n',
            '|\n',
            '----------\n')
    case 2:
      print(' ----------\n'
            ' |        |\n',
            '|        O\n',
            '|        |\n',
            '|\n',
            '|\n',
            '----------\n')
    case 3:
      print(' ----------\n'
            ' |        |\n',
            '|        O\n',
            '|       /|\n',
            '|\n',
            '|\n',
            '----------\n')
    case 4:
      print(' ----------\n'
            ' |        |\n',
            '|        O\n',
            '|       /|\\\n',
            '|\n',
            '|\n',
            '----------\n')
    case 5:
      print(' ----------\n'
            ' |        |\n',
            '|        O\n',
            '|       /|\\\n'
            ' |       /\n',
            '|\n',
            '----------\n')
    case _:
      print(' ----------\n'
            ' |        |\n',
            '|        O\n',
            '|       /|\\\n',
            '|       / \\\n',
            '|\n',
            '----------\n',
            "GAME OVER\n")

def hangmanGame(word: str):
  '''
  Plays hangman game, where you need to guess the word "word".
  '''
  alphabet = list('abcdefghijklmnopqrstuvwxyz')
  nTries = 0
  guessedLetters = ['_' for i in range(len(word))]
  triedLetters = set()
  currentLetter = ''
  print("Hangman: Guess the word!")
  while(currentLetter != 'E'):
    if(nTries < 6):
      printHangman(nTries)
      print(guessedLetters)
      print("Letters tried: ", triedLetters)
      print("Type a letter")

      currentLetter = input()

      # Validating input
      if(currentLetter == 'E'):
        continue
      while(currentLetter not in alphabet):
        print("Not a valid letter")
        currentLetter = input()
      while(currentLetter in triedLetters):
        print("You already tried this letter!")
        currentLetter = input()

      triedLetters.update({currentLetter})
      if(currentLetter in word):
        for i in range(len(guessedLetters)):
          if(currentLetter == word[i]):
            guessedLetters[i] = currentLetter
        if("_" not in guessedLetters):
          print("You win! \nThe word is ", word, " and you missed ",
                nTries, " letters")
          break
        continue
      nTries += 1
    else:
      printHangman(nTries)
      print("Type E to exit")
      currentLetter = input()
      if(currentLetter in alphabet):
        raise ValueError("The game has already ended.")


hangmanGame("caio")