# import modul
from strings_with_arrows import *

# pendefinisian digit/konstanta
DIGITS = '0123456789'

# pendefinisian eror
class Error:
	def __init__(self, pos_start, pos_end, error_name, details):
		self.pos_start = pos_start
		self.pos_end = pos_end
		self.error_name = error_name
		self.details = details
	
    # menampilkan jenis eror
	def as_string(self):
		result  = f'{self.error_name}: {self.details}\n'
		result += f'File {self.pos_start.fn}, line {self.pos_start.ln + 1}'
		result += '\n\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
		return result

# menampilkan pesan eror pada lexer
class IllegalCharError(Error):
	def __init__(self, pos_start, pos_end, details):
		super().__init__(pos_start, pos_end, 'Illegal Character', details)

# menampilkan pesan syntax eror pada parser
class InvalidSyntaxError(Error):
	def __init__(self, pos_start, pos_end, details=''):
		super().__init__(pos_start, pos_end, 'Invalid Syntax', details)

# menampilkan jejak/letak eror 
class RTError(Error):
	def __init__(self, pos_start, pos_end, details, context):
		super().__init__(pos_start, pos_end, 'Runtime Error', details)
		self.context = context

	def as_string(self):
		result  = self.generate_traceback()
		result += f'{self.error_name}: {self.details}'
		result += '\n\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
		return result

	def generate_traceback(self):
		result = ''
		pos = self.pos_start
		ctx = self.context

		while ctx:
			result = f'  File {pos.fn}, line {str(pos.ln + 1)}, in {ctx.display_name}\n' + result
			pos = ctx.parent_entry_pos
			ctx = ctx.parent

		return 'Traceback (most recent call last):\n' + result

# class posisi untuk melacak indeks kolom dan baris saat ini
class Position:
	def __init__(self, idx, ln, col, fn, ftxt):
		self.idx = idx
		self.ln = ln
		self.col = col
		self.fn = fn
		self.ftxt = ftxt

    # fungsi advance untuk pindah ke baris berikutnya
	def advance(self, current_char=None):
		self.idx += 1
		self.col += 1

		if current_char == '\n':
			self.ln += 1
			self.col = 0

		return self

	def copy(self):
		return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)

# pendefinisian token
TT_INT = 'integer'
TT_FLOAT = 'float'
TT_PLUS = 'plus'
TT_MINUS = 'minus'
TT_MUL = 'kali'
TT_DIV = 'bagi'
TT_LPAREN = 'buka kurung'
TT_RPAREN = 'tutup kurung'
TT_EOF = 'token akhir' # token akhir, EOF = End Of File untuk mendeteksi akhir token dari file

# class token
class Token:
     # pendefinisian tipe dan nilai
	def __init__(self, type_, value=None, pos_start=None, pos_end=None):
		self.type = type_
		self.value = value

		if pos_start:
			self.pos_start = pos_start.copy()
			self.pos_end = pos_start.copy()
			self.pos_end.advance()

		if pos_end:
			self.pos_end = pos_end
	
    # representasi tipe dan nilai agar terlihat rapi saat dicetak ke terminal
	def __repr__(self):
		if self.value: return f'{self.type}:{self.value}'
		return f'{self.type}'

# class lexer (analisis leksikal)
class Lexer:
    # melacak posisi karakter
	def __init__(self, fn, text):
		self.fn = fn
		self.text = text
		self.pos = Position(-1, 0, -1, fn, text)
		self.current_char = None
		self.advance()
	
    # pendefinisian metode lanjutan untuk melacak karakter selanjutnya
	def advance(self):
		self.pos.advance(self.current_char)
		self.current_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None
        # memeriksa jumlah text total, jika karakter selanjutnya lebih dari total text maka akan dikembalikan ke None

    # fungsi untuk token
	def make_tokens(self):
		tokens = []

		while self.current_char != None:
			if self.current_char in ' \t':
				self.advance()
			elif self.current_char in DIGITS:
				tokens.append(self.make_number())
			elif self.current_char == '+':
				tokens.append(Token(TT_PLUS, pos_start=self.pos))
				self.advance()
			elif self.current_char == '-':
				tokens.append(Token(TT_MINUS, pos_start=self.pos))
				self.advance()
			elif self.current_char == '*':
				tokens.append(Token(TT_MUL, pos_start=self.pos))
				self.advance()
			elif self.current_char == '/':
				tokens.append(Token(TT_DIV, pos_start=self.pos))
				self.advance()
			elif self.current_char == '(':
				tokens.append(Token(TT_LPAREN, pos_start=self.pos))
				self.advance()
			elif self.current_char == ')':
				tokens.append(Token(TT_RPAREN, pos_start=self.pos))
				self.advance()
			else: # mereturn eror
				pos_start = self.pos.copy()
				char = self.current_char
				self.advance()
				return [], IllegalCharError(pos_start, self.pos, "'" + char + "'")

		tokens.append(Token(TT_EOF, pos_start=self.pos)) #menambahkan token akhir
		return tokens, None

    # fungsi untuk melacak bilangan/angka
	def make_number(self):
		num_str = ''
		dot_count = 0
		pos_start = self.pos.copy()

        # memeriksa karakter selanjutnya, apakah masih ada? apakah masih berupa digit/angka?
		while self.current_char != None and self.current_char in DIGITS + '.':
			if self.current_char == '.':
				if dot_count == 1: break
				dot_count += 1
				num_str += '.'
			else:
				num_str += self.current_char
			self.advance()

        # identifikasi float dan integer (memeriksa adanya titik dalam bilangan)
		if dot_count == 0:
			return Token(TT_INT, int(num_str), pos_start, self.pos)
		else:
			return Token(TT_FLOAT, float(num_str), pos_start, self.pos)

# pendefinisian node
class NumberNode:
	def __init__(self, tok):
		self.tok = tok

		self.pos_start = self.tok.pos_start
		self.pos_end = self.tok.pos_end

    # fungsi pengembalian token dalam string
	def __repr__(self):
		return f'{self.tok}'

# node operasi biner untuk operasi aritmatika (+, -, *, /)
class BinOpNode:
	def __init__(self, left_node, op_tok, right_node):
		self.left_node = left_node
		self.op_tok = op_tok
		self.right_node = right_node

        # menginisialisasi secara default, operasi akan dimulai dari paling kiri dan berakhir pada input paling kanan
		self.pos_start = self.left_node.pos_start
		self.pos_end = self.right_node.pos_end

    # representasi operasi node pada terminal
	def __repr__(self):
		return f'({self.left_node}, {self.op_tok}, {self.right_node})'

# memungkinkan input bilangan negatif dan tanda kurung sebagai penentu prioritas operasi
class UnaryOpNode:
	def __init__(self, op_tok, node):
		self.op_tok = op_tok
		self.node = node

        # menginisialisasi secara default, operasi akan dimulai dari paling kiri dan berakhir pada input paling kanan
		self.pos_start = self.op_tok.pos_start
		self.pos_end = node.pos_end

	def __repr__(self):
		return f'({self.op_tok}, {self.node})'


# pendefinisian hasil parser (eror atau berjalan dengan baik)
class ParseResult:
	def __init__(self):
		self.error = None
		self.node = None

    # mengambil simpul/node lain untuk diperiksa adanya eror pada node tersebut
	def register(self, res):
		if isinstance(res, ParseResult):
			if res.error: self.error = res.error
			return res.node

		return res

    # mengeksekusi node tanpa eror
	def success(self, node):
		self.node = node
		return self

    # mengeksekusi node yang eror
	def failure(self, error):
		self.error = error
		return self


# pendefinisian parser 
class Parser:
	def __init__(self, tokens):
		self.tokens = tokens
		self.tok_idx = -1
		self.advance()

	def advance(self, ):
		self.tok_idx += 1
		if self.tok_idx < len(self.tokens):
			self.current_tok = self.tokens[self.tok_idx]
		return self.current_tok

    # memanggil ekspresi saat ini dan mengembalikannya
	def parse(self):
		res = self.expr()
		if not res.error and self.current_tok.type != TT_EOF:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Expected '+', '-', '*' or '/'"
			))
		return res

	###################################

	def factor(self):
		res = ParseResult()
		tok = self.current_tok

        # memeriksa token termasuk plus atau minus
		if tok.type in (TT_PLUS, TT_MINUS):
			res.register(self.advance())
			factor = res.register(self.factor())
			if res.error: return res
			return res.success(UnaryOpNode(tok, factor))
		
        # memeriksa token termasuk integer atau float
		elif tok.type in (TT_INT, TT_FLOAT):
			res.register(self.advance())
			return res.success(NumberNode(tok))

        # memeriksa adanya tanda kurung untuk prioritas operasi
		elif tok.type == TT_LPAREN:
			res.register(self.advance())
			expr = res.register(self.expr())
			if res.error: return res
			if self.current_tok.type == TT_RPAREN:
				res.register(self.advance())
				return res.success(expr)
			else:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Expected ')'"
				))

		return res.failure(InvalidSyntaxError(
			tok.pos_start, tok.pos_end,
			"Expected int or float"
		))

	def term(self):
		return self.bin_op(self.factor, (TT_MUL, TT_DIV))

	def expr(self):
		return self.bin_op(self.term, (TT_PLUS, TT_MINUS))

	###################################

	def bin_op(self, func, ops):
		res = ParseResult()
		left = res.register(func())
		if res.error: return res

		while self.current_tok.type in ops:
			op_tok = self.current_tok
			res.register(self.advance())
			right = res.register(func())
			if res.error: return res
			left = BinOpNode(left, op_tok, right)

		return res.success(left)

# mengembalikan hasil run = melacak hasil saat ini dan melacak eror nya jika ada
class RTResult:
	def __init__(self):
		self.value = None
		self.error = None

	def register(self, res):
		if res.error: self.error = res.error
		return res.value

	def success(self, value):
		self.value = value
		return self

	def failure(self, error):
		self.error = error
		return self

# menyimpan dan mengoperasikan bilangan
class Number:
	def __init__(self, value):
		self.value = value
		self.set_pos()
		self.set_context()

	def set_pos(self, pos_start=None, pos_end=None):
		self.pos_start = pos_start
		self.pos_end = pos_end
		return self

    # pendeklarasian context
	def set_context(self, context=None):
		self.context = context
		return self

    # menambahkan nilai sebelumnya dengan nilai selanjutnya
	def added_to(self, other):
		if isinstance(other, Number):
			return Number(self.value + other.value).set_context(self.context), None

    # mengurangkan nilai sebelumnya dengan nilai selanjutnya
	def subbed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value - other.value).set_context(self.context), None

    # mengalikan nilai sebelumnya dengan nilai selanjutnya
	def multed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value * other.value).set_context(self.context), None

    # membagikan nilai sebelumnya dengan nilai selanjutnya
	def dived_by(self, other):
		if isinstance(other, Number):
			if other.value == 0:
				return None, RTError(
					other.pos_start, other.pos_end,
					'Tidak terdefinisi : Pembagian dengan nol',
					self.context
				)

			return Number(self.value / other.value).set_context(self.context), None

	def __repr__(self):
		return str(self.value)

# menampung konteks program saat ini
class Context:
	def __init__(self, display_name, parent=None, parent_entry_pos=None):
		self.display_name = display_name
		self.parent = parent
		self.parent_entry_pos = parent_entry_pos

# pendefinisian interpreter / analisis semantik
class Interpreter:
	def visit(self, node, context):
		method_name = f'visit_{type(node).__name__}'
		method = getattr(self, method_name, self.no_visit_method)
		return method(node, context)

	def no_visit_method(self, node, context):
		raise Exception(f'No visit_{type(node).__name__} method defined')

	###################################

	def visit_NumberNode(self, node, context):
		return RTResult().success(
			Number(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
		)

    # mengunjungi mencetak operasi aritmatika pada input
	def visit_BinOpNode(self, node, context):
		res = RTResult()
		left = res.register(self.visit(node.left_node, context))
		if res.error: return res
		right = res.register(self.visit(node.right_node, context))
		if res.error: return res

        # memeriksa jenis operasi yang terdapat pada input kemudian mengiplementasikannya
		if node.op_tok.type == TT_PLUS:
			result, error = left.added_to(right)
		elif node.op_tok.type == TT_MINUS:
			result, error = left.subbed_by(right)
		elif node.op_tok.type == TT_MUL:
			result, error = left.multed_by(right)
		elif node.op_tok.type == TT_DIV:
			result, error = left.dived_by(right)

		if error:
			return res.failure(error)
		else:
			return res.success(result.set_pos(node.pos_start, node.pos_end))

    # memeriksa adanya bilangan negatif atau tanda kurung sebagai penentu prioritas operasi
	def visit_UnaryOpNode(self, node, context):
		res = RTResult()
		number = res.register(self.visit(node.node, context))
		if res.error: return res

		error = None

        # jika terdapat tanda negatif, maka bilangan akan dikalikan dengan -1
		if node.op_tok.type == TT_MINUS:
			number, error = number.multed_by(Number(-1))

		if error:
			return res.failure(error)
		else:
			return res.success(number.set_pos(node.pos_start, node.pos_end))

# fungsi run
def run(fn, text):
	# membuat token
	lexer = Lexer(fn, text)
	tokens, error = lexer.make_tokens()
	if error: return None, error
	
	# membuat pohon parse 
	parser = Parser(tokens)
	ast = parser.parse()
	if ast.error: return None, ast.error

    # memanggil interpreter (analisis semantik)
	interpreter = Interpreter()
	context = Context('<program>')
	result = interpreter.visit(ast.node, context)

	return result.value, result.error