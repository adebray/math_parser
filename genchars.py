#!/usr/bin/python3

# 24 June 2016
# For displaying a range of characters in the Mathematical Alphanumeric Symbols block
# in Unicode. Change the strings to change the output.

for ch in range(ord('a'), ord('z')+1):
	print(chr(ch) + ' ' + chr(ord('𝓪') + ch - ord('a')))
