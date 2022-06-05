from Solver import *


# Initially, we create a new instance of class Model and run Built_Model function.
# Then we call the Solver class getting the above model as parameter.
# The solve function is called to construct a greed algorithm.

m = Model()
m.Build_Model()
s = Solver(m)
solution = s.solve()
