# load packages
import random
import math


class Node:
    # Denotes customer: idd unique number, xx and yy are location details, st is service_time, prof is profit
    def __init__(self, idd, xx, yy, st, prof):
        self.id = idd
        self.x = xx
        self.y = yy
        self.service_time = st
        self.profit = prof
        self.isRouted = False
        self.isTabuTillIterator = -1


class Route:
    # Denotes the route: list sequenceOfNodes will include the customers,
    # Initialize each route to start and end with depot
    def __init__(self, depot, tl):
        self.sequenceOfNodes = []
        self.sequenceOfNodes.append(depot)
        self.sequenceOfNodes.append(depot)
        self.duration = 0    # denotes the summary of distance time between nodes and service time of each node
        self.profit = 0      # denotes the summary of profit of all nodes
        self.timeLimit = tl  # denotes the time limit - set 150


class Model:

    # Instance variables
    def __init__(self):
        self.all_nodes = []      # includes depot and all customers
        self.customers = []      # denotes possible customers to be visited
        self.dist_matrix = []    # distance matrix for each pair
        self.timeLimit = 150     # duration limit for service_time + dist time(matrix) must be <= than 150 each route
        self.number_of_trucks = 5

    def Build_Model(self):

        # Initialize random generator
        birthday = 24121994
        random.seed(birthday)

        self.all_nodes = []
        depot = Node(0, 50, 50, 0, 0)
        self.all_nodes.append(depot)

        # create customers
        total_customers = 300

        # For each customer initialize randomly the coordinates, service time and profit
        for i in range(0, total_customers):
            x = random.randint(0, 100)
            y = random.randint(0, 100)
            service_time = random.randint(5, 10)
            profit = random.randint(5, 20)
            cust = Node(i + 1, x, y, service_time, profit)
            self.all_nodes.append(cust)
            self.customers.append(cust)

        # Create distance matrix
        rows = len(self.all_nodes)
        self.dist_matrix = [[0.0 for x in range(rows)] for y in range(rows)]

        # Calculate distance for each pair of customers
        for i in range(0, len(self.all_nodes)):
            for j in range(0, len(self.all_nodes)):
                a = self.all_nodes[i]
                b = self.all_nodes[j]
                dist = math.sqrt(math.pow(a.x - b.x, 2) + math.pow(a.y - b.y, 2))
                self.dist_matrix[i][j] = dist
