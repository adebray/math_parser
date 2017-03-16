#!/usr/bin/env python3

def line(cmd):
	print('\\' + cmd + ' ' + cmd + ' op')


#for f in ['sin', 'cos', 'tan', 'sec', 'csc', 'cot']: 
#	line(f)
#	line('arc' + f)
#	line(f + 'h')
#	line('arc' + f + 'h')

# more operators

for f in ['Cliff', 'coker', 'End', 'Ext', 'Frac', 'Gal', 'Hom', 'Im', 'ker',
		  'Mat', 'sign', 'Re', 'Sym', 'Tor', 'Proj', 'QCoh', 'res', 'Spec',
		  'codim', 'crit', 'curl', 'div', 'supp', 'det', 'arg', 'deg', 'dim',
		  'exp', 'gcd', 'lcm', 'hom', 'inf', 'lim', 'colim', 'holim', 'hocolim',
		  'lg', 'ln', 'log', 'liminf', 'limsup', 'max', 'min', 'Pr', 'sup',
		  'rank']:
	line(f)
