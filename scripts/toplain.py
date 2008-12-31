#!/usr/bin/env python

"""Convert HTML to ASCII."""

from HTMLParser import HTMLParser
from sys import stdin, stdout, argv
import unicodedata
import htmlentitydefs
import re

PARAGRAPH_INDENT = 2
ITALIC_CHAR = '*'

output = stdout

#stderr = open("unicode_errors.txt", 'w')

space_re = re.compile('\s+')

html_symbols = (
# Entity code, substituted 7-bit letter, description, code
# Source: http://www.cs.tut.fi/~jkorpela/html/guide/entities.html
# Not currently used by the script, printing UTF-8

('nbsp', ' ', "no-break space (non-breaking space)", 160),
('iexcl', '!', "inverted exclamation mark", 161),
('cent', 'c', "cent sign", 162),
('pound', '<pound>', "pound sign", 163),
('curren', '<currency>', "currency sign", 164),
('yen', '<yen>', "yen sign (yuan sign)", 165),
('brvbar', '|', "broken bar (broken vertical bar)", 166),
('sect', 'S', "section sign", 167),
('uml', '', "diaeresis (spacing diaeresis)", 168),
('copy', '(c)', "copyright sign", 169),
('ordf', 'a', "feminine ordinal indicator", 170),
('laquo', '<<', "left-pointing double angle quotation mark (left pointing guillemet)", 171),
('not', '!', "not sign", 172),
('shy', '-', "soft hyphen (discretionary hyphen)", 173),
('reg', '(r)', "registered sign (registered trade mark sign)", 174),
('macr', '~', "macron (spacing macron, overline APL overbar)", 175),
('deg', '<deg>', "degree sign", 176),
('plusmn', '+/-', "plus-minus sign (plus-or-minus sign)", 177),
('sup2', '^2', "superscript two (superscript digit two, squared)", 178),
('sup3', '^3', "superscript three (superscript digit three, cubed)", 179),
('acute', '\'', "acute accent (spacing acute)", 180),
('micro', 'mu', "micro sign", 181),
('para', '<para>', "pilcrow sign (paragraph sign)", 182),
('middot', '*', "middle dot (Georgian comma, Greek middle dot)", 183),
('cedil', '_', "cedilla (spacing cedilla)", 184),
('sup1', '^1', "superscript one (superscript digit one)", 185),
('ordm', 'o', "masculine ordinal indicator", 186),
('raquo', '>>', "right-pointing double angle quotation mark (right pointing guillemet)", 187),
('frac14', '1/4', "vulgar fraction one quarter (fraction one quarter)", 188),
('frac12', '1/2', "vulgar fraction one half (fraction one half)", 189),
('frac34', '3/4', "vulgar fraction three quarters (fraction three quarters)", 190),
('iquest', '?', "inverted question mark (turned question mark)", 191),
('Agrave', 'A', "Latin capital letter A with grave (Latin capital letter A grave)", 192),
('Aacute', 'A', "Latin capital letter A with acute", 193),
('Acirc', 'A', "Latin capital letter A with circumflex", 194),
('Atilde', 'A', "Latin capital letter A with tilde", 195),
('Auml', 'A', "Latin capital letter A with diaeresis", 196),
('Aring', 'A', "Latin capital letter A with ring above (Latin capital letter A ring)", 197),
('AElig', 'AE', "Latin capital letter AE (Latin capital ligature AE)", 198),
('Ccedil', 'C', "Latin capital letter C with cedilla", 199),
('Egrave', 'E', "Latin capital letter E with grave", 200),
('Eacute', 'E', "Latin capital letter E with acute", 201),
('Ecirc', 'E', "Latin capital letter E with circumflex", 202),
('Euml', 'E', "Latin capital letter E with diaeresis", 203),
('Igrave', 'I', "Latin capital letter I with grave", 204),
('Iacute', 'I', "Latin capital letter I with acute", 205),
('Icirc', 'I', "Latin capital letter I with circumflex", 206),
('Iuml', 'I', "Latin capital letter I with diaeresis", 207),
('ETH', 'ETH', "Latin capital letter ETH", 208),
('Ntilde', 'N', "Latin capital letter N with tilde", 209),
('Ograve', 'O', "Latin capital letter O with grave", 210),
('Oacute', 'O', "Latin capital letter O with acute", 211),
('Ocirc', 'O', "Latin capital letter O with circumflex", 212),
('Otilde', 'O', "Latin capital letter O with tilde", 213),
('Ouml', 'O', "Latin capital letter O with diaeresis", 214),
('times', '*', "multiplication sign", 215),
('Oslash', 'O', "Latin capital letter O with stroke (Latin capital letter O slash)", 216),
('Ugrave', 'U', "Latin capital letter U with grave", 217),
('Uacute', 'U', "Latin capital letter U with acute", 218),
('Ucirc', 'U', "Latin capital letter U with circumflex", 219),
('Uuml', 'U', "Latin capital letter U with diaeresis", 220),
('Yacute', 'Y', "Latin capital letter Y with acute", 221),
('THORN', 'TH', "Latin capital letter THORN", 222),
('szlig', 'ss', "Latin small letter sharp s (ess-zed)", 223),
('agrave', 'a', "Latin small letter a with grave (Latin small letter a grave)", 224),
('aacute', 'a', "Latin small letter a with acute", 225),
('acirc', 'a', "Latin small letter a with circumflex", 226),
('atilde', 'a', "Latin small letter a with tilde", 227),
('auml', 'a', "Latin small letter a with diaeresis", 228),
('aring', 'a', "Latin small letter a with ring above (Latin small letter a ring)", 229),
('aelig', 'ae', "Latin small letter ae (Latin small ligature ae)", 230),
('ccedil', 'c', "Latin small letter c with cedilla", 231),
('egrave', 'e', "Latin small letter e with grave", 232),
('eacute', 'e', "Latin small letter e with acute", 233),
('ecirc', 'e', "Latin small letter e with circumflex", 234),
('euml', 'e', "Latin small letter e with diaeresis", 235),
('igrave', 'i', "Latin small letter i with grave", 236),
('iacute', 'i', "Latin small letter i with acute", 237),
('icirc', 'i', "Latin small letter i with circumflex", 238),
('iuml', 'i', "Latin small letter i with diaeresis", 239),
('eth', 'eth', "Latin small letter eth", 240),
('ntilde', 'n', "Latin small letter n with tilde", 241),
('ograve', 'o', "Latin small letter o with grave", 242),
('oacute', 'o', "Latin small letter o with acute", 243),
('ocirc', 'o', "Latin small letter o with circumflex", 244),
('otilde', 'o', "Latin small letter o with tilde", 245),
('ouml', 'o', "Latin small letter o with diaeresis", 246),
('divide', '/', "division sign", 247),
('oslash', 'o', "Latin small letter o with stroke (Latin small letter o slash)", 248),
('ugrave', 'u', "Latin small letter u with grave", 249),
('uacute', 'u', "Latin small letter u with acute", 250),
('ucirc', 'u', "Latin small letter u with circumflex", 251),
('uuml', 'u', "Latin small letter u with diaeresis", 252),
('yacute', 'y', "Latin small letter y with acute", 253),
('thorn', 'th', "Latin small letter thorn with", 254),
('yuml', 'y', "Latin small letter y with diaeresis", 255),
('Alpha', 'Alpha', "Greek capital letter alpha", 913),
('Beta', 'Beta', "Greek capital letter beta", 914),
('Gamma', 'Gamma', "Greek capital letter gamma", 915),
('Delta', 'Delta', "Greek capital letter delta", 916),
('Epsilon', 'Epsilon', "Greek capital letter epsilon", 917),
('Zeta', 'Zeta', "Greek capital letter zeta", 918),
('Eta', 'Eta', "Greek capital letter eta", 919),
('Theta', 'Theta', "Greek capital letter theta", 920),
('Iota', 'Iota', "Greek capital letter iota", 921),
('Kappa', 'Kappa', "Greek capital letter kappa", 922),
('Lambda', 'Lambda', "Greek capital letter lambda", 923),
('Mu', 'Mu', "Greek capital letter mu", 924),
('Nu', 'Nu', "Greek capital letter nu", 925),
('Xi', 'Xi', "Greek capital letter xi", 926),
('Omicron', 'Omicron', "Greek capital letter omicron", 927),
('Pi', 'Pi', "Greek capital letter pi", 928),
('Rho', 'Rho', "Greek capital letter rho", 929),
('Sigma', 'Sigma', "Greek capital letter sigma", 931),
('Tau', 'Tau', "Greek capital letter tau", 932),
('Upsilon', 'Upsilon', "Greek capital letter upsilon", 933),
('Phi', 'Phi', "Greek capital letter phi", 934),
('Chi', 'Chi', "Greek capital letter chi", 935),
('Psi', 'Psi', "Greek capital letter psi", 936),
('Omega', 'Omega', "Greek capital letter omega", 937),
('alpha', 'alpha', "Greek small letter alpha", 945),
('beta', 'beta', "Greek small letter beta", 946),
('gamma', 'gamma', "Greek small letter gamma", 947),
('delta', 'delta', "Greek small letter delta", 948),
('epsilon', 'epsilon', "Greek small letter epsilon", 949),
('zeta', 'zeta', "Greek small letter zeta", 950),
('eta', 'eta', "Greek small letter eta", 951),
('theta', 'theta', "Greek small letter theta", 952),
('iota', 'iota', "Greek small letter iota", 953),
('kappa', 'kappa', "Greek small letter kappa", 954),
('lambda', 'lambda', "Greek small letter lambda", 955),
('mu', 'mu', "Greek small letter mu", 956),
('nu', 'nu', "Greek small letter nu", 957),
('xi', 'xi', "Greek small letter xi", 958),
('omicron', 'omicron', "Greek small letter omicron", 959),
('pi', 'pi', "Greek small letter pi", 960),
('rho', 'rho', "Greek small letter rho", 961),
('sigmaf', 'sigmaf', "Greek small letter final sigma", 962),
('sigma', 'sigma', "Greek small letter sigma", 963),
('tau', 'tau', "Greek small letter tau", 964),
('upsilon', 'upsilon', "Greek small letter upsilon", 965),
('phi', 'phi', "Greek small letter phi", 966),
('chi', 'chi', "Greek small letter chi", 967),
('psi', 'psi', "Greek small letter psi", 968),
('omega', 'omega', "Greek small letter omega", 969),
('thetasym', 'thetasym', "Greek small letter theta symbol", 977),
('upsih', 'upsih', "Greek upsilon with hook symbol", 978),
('piv', 'pi', "Greek pi symbol", 982),
('bull', '*', "bullet (black small circle)", 8226),
('hellip', '...', "horizontal ellipsis (three dot leader)", 8230),
('prime', '\'', "prime (minutes, feet)", 8242),
('Prime', '"', "double prime (seconds, inches)", 8243),
('oline', '-', "overline (spacing overscore)", 8254),
('frasl', '/', "fraction slash", 8260),
('weierp', 'P', "script capital P (power set, Weierstrass p)", 8472),
('image', 'I', "blackletter capital I (imaginary part)", 8465),
('real', 'R', "blackletter capital R (real part symbol)", 8476),
('trade', 'tm', "trade mark sign", 8482),
('alefsym', 'alef', "alef symbol (first transfinite cardinal)", 8501),
('larr', '<-', "leftwards arrow", 8592),
('uarr', '^', "upwards arrow", 8593),
('rarr', '->', "rightwards arrow", 8594),
('darr', 'v', "downwards arrow", 8595),
('harr', '<->', "left right arrow", 8596),
('crarr', '<-"', "downwards arrow with corner leftwards (carriage return)", 8629),
('lArr', '<=', "leftwards double arrow", 8656),
('uArr', '^', "upwards double arrow", 8657),
('rArr', '=>', "rightwards double arrow", 8658),
('dArr', 'v', "downwards double arrow", 8659),
('hArr', '<=>', "left right double arrow", 8660),
('forall', '<forall>', "for all", 8704),
('part', '<part>', "partial differential", 8706),
('exist', '<exist>', "there exists", 8707),
('empty', '0', "empty set (null set, diameter)", 8709),
('nabla', '<nabla>', "nabla (backward difference)", 8711),
('isin', '<in>', "element of", 8712),
('notin', '<notin>', "not an element of", 8713),
('ni', '<ni>', "contains as member", 8715),
('prod', 'P', "n-ary product (product sign)", 8719),
('sum', 'S', "n-ary summation", 8721),
('minus', '-', "minus sign", 8722),
('lowast', '*', "asterisk operator", 8727),
('radic', '<sqrt>', "square root (radical sign)", 8730),
('prop', '<prop>', "proportional to", 8733),
('infin', '<inf>', "infinity", 8734),
('ang', '<ang>', "angle", 8736),
('and', '/\\', "logical and (wedge)", 8743),
('or', '\/', "logical or (vee)", 8744),
('cap', '<cap>', "intersection (cap)", 8745),
('cup', '<cup>', "union (cup)", 8746),
('int', '<int>', "integral", 8747),
('there4', '<there4>', "therefore", 8756),
('sim', '~', "tilde operator (varies with, similar to)", 8764),
('cong', '~=', "approximately equal to", 8773),
('asymp', '~~', "almost equal to (asymptotic to)", 8776),
('ne', '!=', "not equal to", 8800),
('equiv', '==', "identical to", 8801),
('le', '<=', "less-than or equal to", 8804),
('ge', '>=', "greater-than or equal to", 8805),
('sub', '<sub>', "subset of", 8834),
('sup', '<sup>', "superset of", 8835),
('nsub', '<nsub>', "not a subset of", 8836),
('sube', '<sube>', "subset of or equal to", 8838),
('supe', '<supe>', "superset of or equal to", 8839),
('oplus', '(+)', "circled plus (direct sum)", 8853),
('otimes', '(*)', "circled times (vector product)", 8855),
('perp', '_|_', "up tack (orthogonal to, perpendicular)", 8869),
('sdot', '<dot>', "dot operator", 8901),
('lceil', '|"', "left ceiling (APL upstile)", 8968),
('rceil', '"|', "right ceiling", 8969),
('lfloor', '|_', "left floor (APL downstile)", 8970),
('rfloor', '_|', "right floor", 8971),
('lang', '<', "left-pointing angle bracket (bra)", 9001),
('rang', '>', "right-pointing angle bracket (ket)", 9002),
('loz', '<>', "lozenge", 9674),
('spades', '<spades>', "black spade suit", 9824),
('clubs', '<clubs>', "black club suit (shamrock)", 9827),
('hearts', '<hearts>', "black heart suit (valentine)", 9829),
('diams', '<diams>', "black diamond suit", 9830),
('quot', '"', "quotation mark (APL quote)", 34),
('amp', '&', "ampersand", 38),
('lt', '<', "less-than sign", 60),
('gt', '>', "greater-than sign", 62),
('OElig', 'OE', "Latin capital ligature OE", 338),
('oelig', 'oe', "Latin small ligature oe", 339),
('Scaron', 'S', "Latin capital letter S with caron", 352),
('scaron', 's', "Latin small letter s with caron", 353),
('Yuml', 'Y', "Latin capital letter Y with diaeresis", 376),
('circ', '^', "modifier letter circumflex accent", 710),
('tilde', '~', "small tilde", 732),
('ensp', ' ', "en space", 8194),
('emsp', ' ', "em space", 8195),
('thinsp', ' ', "thin space", 8201),
('zwnj', '', "zero width non-joiner", 8204),
('zwj', '', "zero width joiner", 8205),
('lrm', '', "left-to-right mark", 8206),
('rlm', '', "right-to-left mark", 8207),
('ndash', ' - ', "en dash", 8211),
('mdash', ' --- ', "em dash", 8212),
('lsquo', '\'', "left single quotation mark", 8216),
('rsquo', '\'', "right single quotation mark", 8217),
('sbquo', '\'', "single low-9 quotation mark", 8218),
('ldquo', '"', "left double quotation mark", 8220),
('rdquo', '"', "right double quotation mark", 8221),
('bdquo', '"', "double low-9 quotation mark", 8222),
('dagger', '**', "dagger", 8224),
('Dagger', '***', "double dagger", 8225),
('permil', '%.', "per mille sign", 8240),
('lsaquo', '<', "single left-pointing angle quotation mark", 8249),
('rsaquo', '>', "single right-pointing angle quotation mark", 8250),
('euro', 'Euro', "euro sign", 8364),
)

class HTMLToPlainParser(HTMLParser):
  
  def __init__(self):
    # XXX: Needs a feature for specifying the default encoding on cmd line.
    self.encoding = "iso-8859-1"
    self.writing = False
    self.new_paragraph = False

    self.printing_italic = False
    self.in_italic_tag = False

    self.tag_stack = [None]

    HTMLParser.__init__(self)

    self.charsubst = {}

    self.italic_string = ""
    
    for x in html_symbols:
      entity, subst, desc, code = x
      self.charsubst[entity] = subst
      self.charsubst[code] = subst

  def paragraph_check(self):
    if self.new_paragraph:
      #output.write('\n' + PARAGRAPH_INDENT * ' ')
      output.write('\n\n')
      self.new_paragraph = False

  def peek_tag(self):
    return self.tag_stack[-1]

  def push_tag(self, tag, attrs):
    self.tag_stack.append((tag, attrs))

  def pop_tag(self):
    del self.tag_stack[-1]
    assert len(self.tag_stack) > 0

  def is_italic_tag(self, (tag, attrs)):
    if tag in ('i', 'I', 'em', 'EM'):
      return True

    for name, value in attrs:
      if value == 'italic':
        return True

    return False

  def handle_starttag(self, tag, attrs):
    self.push_tag(tag, attrs)
    if tag == 'meta':
      self.look_for_charset(attrs)
    if tag == 'body':
      self.writing = True
    if not self.writing:
      return

    if tag in ('p', 'h1', 'h2', 'h3', 'h4', 'br'):
      self.new_paragraph = True

    #if tag == 'br':
      #output.write('\n' + PARAGRAPH_INDENT * ' ')

    if self.is_italic_tag(self.peek_tag()):
      self.in_italic_tag = True
      self.italic_string = ""

  def look_for_charset(self, attrs):
    """Find the character encoding for the HTML file.
    
    Kind of brute force, just looks for a suitable definition string in all
    meta attribute values, doesn't check whether it's actually supposed to be
    a Content-Type declaration. """

    attrs = dict(attrs)
    charset_re = re.compile(r"\bcharset\s*=\s*([^\s]+)")
    for x in attrs.values():
      res = re.search(charset_re, x)
      if res:
        encoding = res.group(1)
        self.encoding = encoding

  def close_italic(self):
    self.in_italic_tag = False
    chars = list(self.italic_string)

    # Scan the string in the italic buffer and don't include heading
    # or trailing nonalphanumeric characters between the * * markers.
    begin = 0
    for i in range(len(chars)):
      if chars[i].isalnum():
        begin = i
        break

    end = len(chars)
    for i in range(len(chars), 0, -1):
      if chars[i - 1].isalnum():
        end = i
        break

    #chars.insert(end, u'*')
    #chars.insert(begin, u'*')
    chars.insert(end, u'>')
    chars.insert(begin, u'<')

    self.write_utf8(''.join(chars))


  def handle_endtag(self, tag):
    if self.is_italic_tag(self.peek_tag()):
      self.close_italic()
    self.pop_tag()

  def handle_data(self, data):
    if not self.writing: return
    if not data.strip(): return
    self.paragraph_check()

    # Turn any number of white space chars into a single space.
    data = space_re.sub(' ', data)
    self.write(data)

  def write_utf8(self, string):
    # An italic tag sometimes encompasses non-alphanumeric chars after the
    # word italicized, so a special buffer hack must be used to ensure the
    # closing italic marker goes to the right place.

    output.write(string.encode('utf-8'))

  def write(self, data):
    """Takes integer unicode codes, unicode strings and ascii strings."""
    if not self.writing: return
    self.paragraph_check()

    string = ''
    if type(data) == int:
    # Code
      if data >= 32:
        string = unichr(data)
    if type(data) == unicode:
      string = data
    if type(data) == str:
      string = unicode(data, self.encoding)

    if self.in_italic_tag:
      self.italic_string += string
    else:
      self.write_utf8(string)

  def handle_charref(self, name):
    # Handle hex
    if name[0] == 'x':
      num = int(name[1:], 16)
    else:
      num = int(name)
    self.write(num)

  def handle_entityref(self, name):
    self.write(htmlentitydefs.name2codepoint[name])

def main(filename):
  parser = HTMLToPlainParser()
  if filename == '-':
    file = stdin
  else:
    file = open(filename)
  parser.feed(file.read())

if __name__ == '__main__':
  if len(argv) > 1:
    file = argv[1]
  else:
    file = '-'
  main(file)
