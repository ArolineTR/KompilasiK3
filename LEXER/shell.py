from distutils.log import error
from unittest import result
from matplotlib.pyplot import text


import basic

while True:
    text = input('lexer>')
    result, error = basic.run('<stdin>', text)

    if error: print(error.as_string())
    else : print(result)
