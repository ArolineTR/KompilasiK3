import estuse

while True:
    text = input('estuse > ')
    result, error = estuse.run('<stdin> ',text)

    if error: print(error.as_string())
    else: print(result)
    