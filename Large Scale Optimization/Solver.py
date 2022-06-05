from VRP_Model import *
from SolutionDrawer import *


# Denotes our solution
class Solution:
    def __init__(self):
        self.profit = 0.0  # firstly we want to maximize the profit
        self.cost = 0.0  # secondly we want to minimize the cost
        self.routes = []


class RelocationMove(object):
    def __init__(self):
        self.originRoutePosition = None
        self.targetRoutePosition = None
        self.originNodePosition = None
        self.targetNodePosition = None
        self.costChangeOriginRt = None
        self.costChangeTargetRt = None
        self.profChangeOriginRt = None  # added
        self.profChangeTargetRt = None  # added
        self.moveCost = None
        self.moveProfit = None  # added

    def Initialize(self):
        self.originRoutePosition = None
        self.targetRoutePosition = None
        self.originNodePosition = None
        self.targetNodePosition = None
        self.costChangeOriginRt = None
        self.costChangeTargetRt = None
        self.profChangeOriginRt = None  # added
        self.profChangeTargetRt = None  # added
        self.moveCost = 10 ** 9
        self.moveProfit = 0  # added


class SwapMove(object):
    def __init__(self):
        self.positionOfFirstRoute = None
        self.positionOfSecondRoute = None
        self.positionOfFirstNode = None
        self.positionOfSecondNode = None
        self.costChangeFirstRt = None
        self.costChangeSecondRt = None
        self.profChangeFirstRt = None  # added
        self.profChangeSecondRt = None  # added
        self.moveCost = None
        self.moveProfit = None  # added

    def Initialize(self):
        self.positionOfFirstRoute = None
        self.positionOfSecondRoute = None
        self.positionOfFirstNode = None
        self.positionOfSecondNode = None
        self.costChangeFirstRt = None
        self.costChangeSecondRt = None
        self.profChangeFirstRt = None  # added
        self.profChangeSecondRt = None  # added
        self.moveCost = 10 ** 9
        self.moveProfit = 0  # added


class UnroutedInsertionMove(object):
    def __init__(self):
        self.customer = None
        self.targetRoutePosition = None
        self.targetNodePosition = None
        self.moveCost = None
        self.moveProfit = None  # added

    def Initialize(self):
        self.customer = None
        self.targetRoutePosition = None
        self.targetNodePosition = None
        self.moveCost = 10 ** 9
        self.moveProfit = 0  # added


class ProfitableSwapMove(object):
    def __init__(self):
        self.customer = None
        self.positionOfTargetRoute = None
        self.positionOfTargetNode = None
        self.moveCost = None
        self.moveProfit = None  # added

    def Initialize(self):
        self.customer = None
        self.positionOfTargetRoute = None
        self.positionOfTargetNode = None
        self.moveCost = 10 ** 9
        self.moveProfit = 0  # added


class CustomerInsertionAllPositions(object):
    def __init__(self):
        self.customer = None
        self.route = None
        self.insertionRoute = None  # check candidate route
        self.insertionPosition = None
        self.cost = 10 ** 9
        self.profit = 0


class Solver:
    def __init__(self, m):
        self.allNodes = m.all_nodes
        self.customers = m.customers
        # self.customers = sorted(m.customers, key=lambda x: x.profit / x.service_time, reverse=True)
        self.depot = m.all_nodes[0]
        self.distanceMatrix = m.dist_matrix
        self.timeLimit = m.timeLimit
        self.number_of_trucks = m.number_of_trucks
        self.sol = None
        self.bestSolution = None
        self.minTabuTenure = 50  # will be used in the tabu
        self.maxTabuTenure = 60  # will be used in the tabu
        self.tabuTenure = 20  # will be used in the tabu
        self.searchTrajectory = []  # will be used for drawing the plot
        self.searchProfitTrajectory = []  # will be used for drawing the plot

    def solve(self):
        self.SetRoutedFlagToFalseForAllCustomers()
        # Question B
        print("~~~~~~~~ MINIMUM-INSERTIONS ~~~~~~~~~")
        self.MinimumInsertions()
        self.ReportSolution(self.sol)
        # Question C
        # print("~~~~~~~~LOCAL-SEARCH~~~~~~~~~")
        # self.LocalSearch(1)
        # self.ReportSolution(self.sol)
        # Question D
        print("~~~~~~~~VND~~~~~~~~~")
        self.VND()
        self.ReportSolution(self.sol)
        return self.sol

    # Initialize all customers with False flag
    def SetRoutedFlagToFalseForAllCustomers(self):
        for i in range(0, len(self.customers)):
            self.customers[i].isRouted = False

    # From the beginning all routes should be open
    def OpenAllRoutes(self):
        while len(self.sol.routes) < self.number_of_trucks:
            rt = Route(self.depot, self.timeLimit)
            self.sol.routes.append(rt)

    def MinimumInsertions(self):
        self.sol = Solution()  # call Solution class
        self.OpenAllRoutes()
        total_duration_routes = 0
        model_is_feasible = True

        # Check that the total duration will not be violated
        while total_duration_routes < (self.number_of_trucks * self.timeLimit):
            best_insertion = CustomerInsertionAllPositions()
            self.IdentifyMinimumCostInsertion(best_insertion)

            if best_insertion.customer is not None:
                self.ApplyCustomerInsertionAllPositions(best_insertion)
                total_duration_routes += best_insertion.cost

            # If the feasibility is violated
            else:
                if model_is_feasible:
                    self.TestSolution()
                print('There is no other best insertion for a feasible model')
                model_is_feasible = False
                break

    # This function checks every customer in the algorithm and calculates if his service time
    # respects the time limit and if he does maximizes the profit
    def IdentifyMinimumCostInsertion(self, best_insertion):
        # best_shaking_moves was used to add random improving
        # best_shaking_moves = []
        for i in range(0, len(self.customers)):
            candidateCust: Node = self.customers[i]
            if candidateCust.isRouted is False:
                for k in range(0, len(self.sol.routes)):
                    candidateRoute: Route = self.sol.routes[k]
                    # 1st check to avoid violation of time limit
                    if candidateRoute.duration + candidateCust.service_time <= candidateRoute.timeLimit:
                        for j in range(0, len(candidateRoute.sequenceOfNodes) - 1):
                            A = candidateRoute.sequenceOfNodes[j]
                            B = candidateRoute.sequenceOfNodes[j + 1]
                            costAdded = self.distanceMatrix[A.id][candidateCust.id] + \
                                        self.distanceMatrix[candidateCust.id][B.id] + \
                                        candidateCust.service_time
                            costRemoved = self.distanceMatrix[A.id][B.id]
                            trialCost = costAdded - costRemoved
                            trialProf = candidateCust.profit
                            # 2nd check to avoid violation of time limit including the extra cost
                            if candidateRoute.duration + candidateCust.service_time + trialCost <= candidateRoute.timeLimit:
                                if trialCost < best_insertion.cost and trialProf > best_insertion.profit:
                                    # if the candidate customer is selected store his new values to the solution
                                    best_insertion.customer = candidateCust
                                    best_insertion.route = candidateRoute
                                    best_insertion.insertionRoute = k
                                    best_insertion.insertionPosition = j
                                    best_insertion.cost = trialCost
                                    best_insertion.profit = trialProf

                                # commented as it does not return a better profit increase
                                #     if trialCost < 0 and trialProf > 0:
                                #         best_shaking_moves.append((best_insertion.customer, best_insertion.route,
                                #                                    best_insertion.insertionRoute,
                                #                                    best_insertion.insertionPosition,
                                #                                    best_insertion.cost, best_insertion.profit))
                                # if len(best_shaking_moves) == 0:
                                #     return
                                # best_move_tuple = sorted(best_shaking_moves[random.randint(0, len(best_shaking_moves) - 1)],
                                #                          key=best_insertion.profit, reverse=True)
                                # self.StoreBestRelocationMove(best_move_tuple[0], best_move_tuple[1], best_move_tuple[2])
                            else:
                                continue
                    else:
                        continue

    # Update the solution with the results of the IdentifyMinimumCostInsertion function
    def ApplyCustomerInsertionAllPositions(self, insertion):
        insCustomer = insertion.customer
        # rt = insertion.route
        insIndex = insertion.insertionPosition
        insRouteIndex = insertion.insertionRoute  # in which route he will be inserted
        self.sol.routes[insRouteIndex].sequenceOfNodes.insert(insIndex + 1, insCustomer)
        self.sol.routes[insRouteIndex].duration += insertion.cost
        self.sol.routes[insRouteIndex].profit += insertion.profit
        self.sol.cost += insertion.cost
        self.sol.profit += insCustomer.profit
        insCustomer.isRouted = True

    # This functions presents the output after implementation of
    # Minimum Insertions, Local Search and VND algorithms
    def ReportSolution(self, sol):
        for i in range(0, len(sol.routes)):
            rt = sol.routes[i]
            print('The route', i, 'consist of nodes:')
            for j in range(0, len(rt.sequenceOfNodes)):
                print(rt.sequenceOfNodes[j].id, end=' ')
            print('With total profit', round(rt.profit), 'and total duration', round(rt.duration))
        print('The final profit is:', round(self.sol.profit))
        print('The final cost is:', round(self.sol.cost))

    # This function is for running local search operators: relocation, swap, insertion, profitable swap
    def LocalSearch(self, operator):
        self.bestSolution = self.cloneSolution(self.sol)
        terminationCondition = False
        localSearchIterator = 0  # it is necessary for the VND

        # Create an instance of each operator
        rm = RelocationMove()
        sm = SwapMove()
        uim = UnroutedInsertionMove()
        psm = ProfitableSwapMove()

        while terminationCondition is False:

            self.InitializeOperators(rm, sm, uim, psm)

            # For each operator initially we identify the best move,
            # we store it and finally we apply it to our solution

            # Relocation Move
            if operator == 0:
                self.FindBestRelocationMove(rm, localSearchIterator)
                if rm.originRoutePosition is not None:
                    if rm.moveCost < 0:
                        self.ApplyRelocationMove(rm, localSearchIterator)
                    else:
                        terminationCondition = True
            # Swap Move
            elif operator == 1:
                self.FindBestSwapMove(sm, localSearchIterator)
                if sm.positionOfFirstRoute is not None:
                    if sm.moveCost < 0:
                        self.ApplySwapMove(sm, localSearchIterator)
                    else:
                        terminationCondition = True
            # Unrouted Insertion Move
            elif operator == 2:
                self.FindBestUnroutedInsertionMove(uim, localSearchIterator)
                if uim.customer is not None:
                    if uim.moveCost < 0:
                        self.ApplyUnroutedInsertionMove(uim, localSearchIterator)
                    else:
                        terminationCondition = True
            # Profitable Swap Move
            elif operator == 3:
                self.FindBestProfitableSwapMove(psm, localSearchIterator)
                if psm.customer is not None:
                    if psm.moveCost < 0:
                        self.ApplyProfitableSwapMove(psm, localSearchIterator)
                    else:
                        terminationCondition = True

            # If we find a better result we apply it to the final cost
            if self.sol.cost < self.bestSolution.cost and self.sol.profit > self.bestSolution.profit:
                self.bestSolution = self.cloneSolution(self.sol)

        # Final results of the solution
        self.sol = self.bestSolution

    # Create a clone of the initial solution with Minimum Insertions
    # to improve it by applying local search operators
    def cloneRoute(self, rt: Route):
        cloned = Route(self.depot, self.timeLimit)
        cloned.duration = rt.duration
        cloned.profit = rt.profit
        cloned.sequenceOfNodes = rt.sequenceOfNodes.copy()
        return cloned

    def cloneSolution(self, sol: Solution):
        cloned = Solution()
        for i in range(0, len(sol.routes)):
            rt = sol.routes[i]
            clonedRoute = self.cloneRoute(rt)
            cloned.routes.append(clonedRoute)
        cloned.cost = self.sol.cost
        cloned.profit = self.sol.profit
        return cloned

    def InitializeOperators(self, rm, sm, uim, psm):
        rm.Initialize()
        sm.Initialize()
        uim.Initialize()
        psm.Initialize()

    # For each visited customer check that if we relocate him to another position,
    # if the total cost of the solution will be reduced depending on his distance from the before and after customer
    def FindBestRelocationMove(self, rm, iterator):
        for originRouteIndex in range(0, len(self.sol.routes)):
            rt1: Route = self.sol.routes[originRouteIndex]
            for targetRouteIndex in range(0, len(self.sol.routes)):
                rt2: Route = self.sol.routes[targetRouteIndex]
                for originNodeIndex in range(1, len(rt1.sequenceOfNodes) - 1):
                    for targetNodeIndex in range(0, len(rt2.sequenceOfNodes) - 1):
                        # If the relocation is in the exact same route and node ignore and continue to other candidate
                        if originRouteIndex == targetRouteIndex and (
                                targetNodeIndex == originNodeIndex or targetNodeIndex == originNodeIndex - 1):
                            continue

                        # Initialize starting positions
                        # B will be positioned between F and G
                        A = rt1.sequenceOfNodes[originNodeIndex - 1]
                        B = rt1.sequenceOfNodes[originNodeIndex]
                        C = rt1.sequenceOfNodes[originNodeIndex + 1]

                        # Initialize targeting positions
                        F = rt2.sequenceOfNodes[targetNodeIndex]
                        G = rt2.sequenceOfNodes[targetNodeIndex + 1]

                        if rt1 != rt2:
                            # 1st check that time limit is not violated
                            if rt2.duration + B.service_time > rt2.timeLimit:
                                continue

                        # Does not include service time of the customer
                        costAdded = self.distanceMatrix[A.id][C.id] + self.distanceMatrix[F.id][B.id] + \
                                    self.distanceMatrix[B.id][G.id]
                        costRemoved = self.distanceMatrix[A.id][B.id] + self.distanceMatrix[B.id][C.id] + \
                                      self.distanceMatrix[F.id][G.id]

                        originRtCostChange = self.distanceMatrix[A.id][C.id] - self.distanceMatrix[A.id][B.id] - \
                                             self.distanceMatrix[B.id][C.id] - B.service_time
                        targetRtCostChange = self.distanceMatrix[F.id][B.id] + self.distanceMatrix[B.id][G.id] - \
                                             self.distanceMatrix[F.id][G.id] + B.service_time

                        originRtProfChange = rt1.profit - B.profit
                        targetRtProfChange = rt2.profit + B.profit

                        moveCost = costAdded - costRemoved
                        moveProfit = B.profit

                        # 2nd check that time limit is not violated including moveCost
                        if rt2.duration + B.service_time + moveCost > rt2.timeLimit:
                            continue

                        if self.MoveIsTabu(B, iterator, moveCost):
                            continue

                        if (moveCost < rm.moveCost) and abs(moveCost) > 0.0001:
                            self.StoreBestRelocationMove(originRouteIndex, targetRouteIndex,
                                                         originNodeIndex, targetNodeIndex,
                                                         moveCost, moveProfit,
                                                         originRtCostChange, targetRtCostChange,
                                                         originRtProfChange, targetRtProfChange, rm)

        return rm.originRoutePosition

    def StoreBestRelocationMove(self, originRouteIndex, targetRouteIndex, originNodeIndex, targetNodeIndex,
                                moveCost, moveProfit,
                                originRtCostChange, targetRtCostChange,
                                originRtProfChange, targetRtProfChange, rm: RelocationMove):
        rm.originRoutePosition = originRouteIndex
        rm.originNodePosition = originNodeIndex
        rm.targetRoutePosition = targetRouteIndex
        rm.targetNodePosition = targetNodeIndex
        rm.costChangeOriginRt = originRtCostChange
        rm.costChangeTargetRt = targetRtCostChange
        rm.profChangeOriginRt = originRtProfChange
        rm.profChangeTargetRt = targetRtProfChange
        rm.moveCost = targetRtCostChange - originRtCostChange
        rm.moveProfit = 0  # profit does not change in overall solution - no new customers

    # Apply the results from the FindBestRelocationMove to the final solution
    # We are expecting that the profit will not be increased as we have the same visited customers
    def ApplyRelocationMove(self, rm: RelocationMove, iterator):

        oldCost = self.CalculateTotalCost(self.sol)

        originRt = self.sol.routes[rm.originRoutePosition]
        targetRt = self.sol.routes[rm.targetRoutePosition]

        B = originRt.sequenceOfNodes[rm.originNodePosition]  # o node pou tha ginei relocate

        if originRt == targetRt:
            del originRt.sequenceOfNodes[rm.originNodePosition]
            if rm.originNodePosition < rm.targetNodePosition:
                targetRt.sequenceOfNodes.insert(rm.targetNodePosition, B)
            else:
                targetRt.sequenceOfNodes.insert(rm.targetNodePosition + 1, B)

            originRt.duration += rm.costChangeOriginRt
            originRt.profit = rm.profChangeOriginRt

            targetRt.duration += rm.costChangeTargetRt
            targetRt.profit = rm.profChangeTargetRt

        else:
            del originRt.sequenceOfNodes[rm.originNodePosition]
            targetRt.sequenceOfNodes.insert(rm.targetNodePosition + 1, B)
            originRt.duration += rm.costChangeOriginRt
            targetRt.duration += rm.costChangeTargetRt
            originRt.profit -= B.profit
            targetRt.profit += B.profit

        self.sol.cost = self.sol.cost + rm.costChangeOriginRt + rm.costChangeTargetRt

        newCost = self.CalculateTotalCost(self.sol)

        self.SetTabuIterator(B, iterator)
        # debuggingOnly
        if abs((newCost - oldCost) - rm.moveCost) > 0.0001:
            print('Cost Issue')
        # self.TestSolution()

    # For each visited customer check that if we swap him with another customer from the same or different route,
    # if the total cost of the solution will be reduced depending on his distance from the before and after customer
    def FindBestSwapMove(self, sm, iterator):
        for firstRouteIndex in range(0, len(self.sol.routes)):
            rt1: Route = self.sol.routes[firstRouteIndex]
            for secondRouteIndex in range(firstRouteIndex, len(self.sol.routes)):
                rt2: Route = self.sol.routes[secondRouteIndex]
                for firstNodeIndex in range(1, len(rt1.sequenceOfNodes) - 1):  # to exclude depot
                    startOfSecondNodeIndex = 1  # initialize with 1 to exclude depot
                    if rt1 == rt2:
                        startOfSecondNodeIndex = firstNodeIndex + 1  # to avoid swap with himself
                    # Exclude depot at target route
                    for secondNodeIndex in range(startOfSecondNodeIndex, len(rt2.sequenceOfNodes) - 1):

                        # Initialize position of first customer
                        a1 = rt1.sequenceOfNodes[firstNodeIndex - 1]
                        b1 = rt1.sequenceOfNodes[firstNodeIndex]
                        c1 = rt1.sequenceOfNodes[firstNodeIndex + 1]

                        # Initialize position of second customer
                        a2 = rt2.sequenceOfNodes[secondNodeIndex - 1]
                        b2 = rt2.sequenceOfNodes[secondNodeIndex]
                        c2 = rt2.sequenceOfNodes[secondNodeIndex + 1]

                        moveCost = None
                        moveProfit = None  # added
                        costChangeFirstRoute = None
                        costChangeSecondRoute = None
                        profChangeFirstRoute = None  # added
                        profChangeSecondRoute = None  # added

                        if rt1 == rt2:
                            if firstNodeIndex == secondNodeIndex - 1:
                                costRemoved = self.distanceMatrix[a1.id][b1.id] + self.distanceMatrix[b1.id][b2.id] + \
                                              self.distanceMatrix[b2.id][c2.id]
                                costAdded = self.distanceMatrix[a1.id][b2.id] + self.distanceMatrix[b2.id][b1.id] + \
                                            self.distanceMatrix[b1.id][c2.id]
                                moveCost = costAdded - costRemoved
                            else:
                                costRemoved1 = self.distanceMatrix[a1.id][b1.id] + self.distanceMatrix[b1.id][c1.id]
                                costAdded1 = self.distanceMatrix[a1.id][b2.id] + self.distanceMatrix[b2.id][c1.id]
                                costRemoved2 = self.distanceMatrix[a2.id][b2.id] + self.distanceMatrix[b2.id][c2.id]
                                costAdded2 = self.distanceMatrix[a2.id][b1.id] + self.distanceMatrix[b1.id][c2.id]
                                moveCost = costAdded1 + costAdded2 - (costRemoved1 + costRemoved2)
                        else:
                            # 1st check at service time of the 1st route
                            if rt1.duration - b1.service_time + b2.service_time > self.timeLimit:
                                continue
                            # 1st check at service time of the 2nd route
                            if rt2.duration - b2.service_time + b1.service_time > self.timeLimit:
                                continue

                            costRemoved1 = self.distanceMatrix[a1.id][b1.id] + self.distanceMatrix[b1.id][c1.id]
                            costAdded1 = self.distanceMatrix[a1.id][b2.id] + self.distanceMatrix[b2.id][c1.id]
                            costRemoved2 = self.distanceMatrix[a2.id][b2.id] + self.distanceMatrix[b2.id][c2.id]
                            costAdded2 = self.distanceMatrix[a2.id][b1.id] + self.distanceMatrix[b1.id][c2.id]

                            costChangeFirstRoute = costAdded1 - costRemoved1 - b1.service_time + b2.service_time
                            costChangeSecondRoute = costAdded2 - costRemoved2 - b2.service_time + b1.service_time

                            profChangeFirstRoute = rt1.profit + b2.profit - b1.profit
                            profChangeSecondRoute = rt2.profit + b1.profit - b2.profit

                            moveCost = costAdded1 + costAdded2 - (costRemoved1 + costRemoved2)

                            # 2nd check including moveCost for 1st route
                            if rt1.duration - b1.service_time + b2.service_time + moveCost > rt1.timeLimit:
                                continue
                            # 2nd check including moveCost for 2nd route
                            if rt2.duration + b1.service_time - b2.service_time + moveCost > rt2.timeLimit:
                                continue

                        if self.MoveIsTabu(b1, iterator, moveCost) or self.MoveIsTabu(b2, iterator, moveCost):
                            continue

                        if moveCost < sm.moveCost and abs(moveCost) > 0.0001:
                            self.StoreBestSwapMove(firstRouteIndex, secondRouteIndex,
                                                   firstNodeIndex, secondNodeIndex,
                                                   moveCost, moveProfit,
                                                   costChangeFirstRoute, costChangeSecondRoute,
                                                   profChangeFirstRoute, profChangeSecondRoute, sm)

    def StoreBestSwapMove(self, firstRouteIndex, secondRouteIndex,
                          firstNodeIndex, secondNodeIndex,
                          moveCost, moveProfit,
                          costChangeFirstRoute, costChangeSecondRoute,
                          profChangeFirstRoute, profChangeSecondRoute, sm):
        sm.positionOfFirstRoute = firstRouteIndex
        sm.positionOfSecondRoute = secondRouteIndex
        sm.positionOfFirstNode = firstNodeIndex
        sm.positionOfSecondNode = secondNodeIndex
        sm.costChangeFirstRt = costChangeFirstRoute
        sm.costChangeSecondRt = costChangeSecondRoute
        sm.moveCost = moveCost
        sm.moveProfit = moveProfit
        sm.profChangeFirstRt = profChangeFirstRoute
        sm.profChangeSecondRt = profChangeSecondRoute

    # Apply the results from the FindBestSwapMove to the final solution
    # We are expecting that the profit will not be increased as we have the same visited customers
    def ApplySwapMove(self, sm, iterator):
        oldCost = self.CalculateTotalCost(self.sol)

        rt1 = self.sol.routes[sm.positionOfFirstRoute]
        rt2 = self.sol.routes[sm.positionOfSecondRoute]
        b1 = rt1.sequenceOfNodes[sm.positionOfFirstNode]
        b2 = rt2.sequenceOfNodes[sm.positionOfSecondNode]
        rt1.sequenceOfNodes[sm.positionOfFirstNode] = b2
        rt2.sequenceOfNodes[sm.positionOfSecondNode] = b1

        if rt1 == rt2:
            rt1.duration += sm.moveCost
        else:
            rt1.duration += sm.costChangeFirstRt
            rt2.duration += sm.costChangeSecondRt
            rt1.profit = sm.profChangeFirstRt
            rt2.profit = sm.profChangeSecondRt

        self.sol.cost += sm.moveCost

        newCost = self.CalculateTotalCost(self.sol)

        self.SetTabuIterator(b1, iterator)
        self.SetTabuIterator(b2, iterator)

        if abs((newCost - oldCost) - sm.moveCost) > 0.0001:
            print('Cost Issue')

    # For each unvisited customer check that if we insert him to our solution,
    # if the total cost of the solution will be reduced depending on his distance from the before and after customer
    # and more specific if the final profit will be increased with the profit of the new customer
    def FindBestUnroutedInsertionMove(self, uim, iterator):
        for i in range(0, len(self.customers)):
            candidateCust: Node = self.customers[i]
            if candidateCust.isRouted is False:
                for targetRouteIndex in range(0, len(self.sol.routes)):
                    rt: Route = self.sol.routes[targetRouteIndex]
                    # 1st check that the time limit is not violated with the added service time
                    if rt.duration + candidateCust.service_time > rt.timeLimit:
                        continue

                    for targetNodeIndex in range(0, len(rt.sequenceOfNodes) - 1):
                        # Initialize positions
                        A = rt.sequenceOfNodes[targetNodeIndex]
                        B = rt.sequenceOfNodes[targetNodeIndex + 1]

                        # Calculate the move cost and move profit
                        costAdded = self.distanceMatrix[A.id][candidateCust.id] + \
                                    self.distanceMatrix[candidateCust.id][B.id] + candidateCust.service_time
                        costRemoved = self.distanceMatrix[A.id][B.id]

                        moveCost = costAdded - costRemoved
                        moveProfit = candidateCust.profit

                        # 2nd check that the time limit is not violated including the move cost
                        if rt.duration + moveCost > rt.timeLimit:
                            continue

                        if self.MoveIsTabu(candidateCust, iterator, moveCost):
                            continue

                        if moveCost < uim.moveCost and abs(moveCost) > 0.0001 and moveProfit > uim.moveProfit:
                            self.StoreBestUnroutedInsertionMove(candidateCust, targetRouteIndex, targetNodeIndex,
                                                                moveCost, moveProfit, uim)

    def StoreBestUnroutedInsertionMove(self, candidateCust, targetRouteIndex, targetNodeIndex, moveCost, moveProfit,
                                       uim: UnroutedInsertionMove):
        uim.customer = candidateCust
        uim.targetRoutePosition = targetRouteIndex
        uim.targetNodePosition = targetNodeIndex
        uim.moveCost = moveCost
        uim.moveProfit = moveProfit

    # Apply the results from the FindBestUnroutedInsertionMove to the final solution
    # We are expecting that the profit will be increased as a new unvisited customer may be inserted
    def ApplyUnroutedInsertionMove(self, uim: UnroutedInsertionMove, iterator):

        oldCost = self.CalculateTotalCost(self.sol)
        oldProfit = self.CalculateTotalProfit(self.sol)

        targetRt = self.sol.routes[uim.targetRoutePosition]
        B = uim.customer  # denotes the node , in which we relocate
        targetRt.sequenceOfNodes.insert(uim.targetNodePosition + 1, B)  # position that the relocate will be set
        targetRt.duration += uim.moveCost
        targetRt.profit += uim.moveProfit

        self.sol.cost += uim.moveCost
        self.sol.profit += uim.moveProfit

        newCost = self.CalculateTotalCost(self.sol)
        newProfit = self.CalculateTotalProfit(self.sol)

        self.SetTabuIterator(B, iterator)

        if abs((newCost - oldCost) - uim.moveCost) > 0.0001:
            print('Cost Issue')
            # self.TestSolution()
        if abs((newProfit - oldProfit) + uim.moveProfit) < 0.0001:
            print("Profit Issue")

    # For each unvisited customer check that if we swap him with another visited customer from the same or
    # different route, if the total cost of the solution will be reduced depending on his distance from the
    # before and after customer and more specific if the final profit will be increased with the profit
    # of the new unvisited customer
    def FindBestProfitableSwapMove(self, psm, iterator):
        for i in range(0, len(self.customers)):
            candidateCust: Node = self.customers[i]
            if candidateCust.isRouted is False:
                for targetRouteIndex in range(0, len(self.sol.routes)):
                    rt: Route = self.sol.routes[targetRouteIndex]
                    for targetNodeIndex in range(1, len(rt.sequenceOfNodes) - 1):

                        a1 = rt.sequenceOfNodes[targetNodeIndex - 1]
                        b1 = rt.sequenceOfNodes[targetNodeIndex]
                        c1 = rt.sequenceOfNodes[targetNodeIndex + 1]

                        moveCost = None
                        moveProfit = None  # added

                        costRemoved = self.distanceMatrix[a1.id][b1.id] + self.distanceMatrix[b1.id][c1.id] + \
                                      b1.service_time
                        costAdded = self.distanceMatrix[a1.id][candidateCust.id] + \
                                    self.distanceMatrix[candidateCust.id][c1.id] + candidateCust.service_time
                        moveCost = costAdded - costRemoved

                        profitRemoved = b1.profit
                        profitAdded = candidateCust.profit
                        moveProfit = profitAdded - profitRemoved

                        if rt.duration + moveCost > rt.timeLimit:
                            continue

                        if self.MoveIsTabu(candidateCust, iterator, moveCost):
                            continue

                        if moveCost < psm.moveCost and abs(moveCost) > 0.0001 and moveProfit > psm.moveProfit:
                            self.StoreBestProfitableSwapMove(candidateCust, targetRouteIndex, targetNodeIndex,
                                                             moveCost, moveProfit, psm)

    def StoreBestProfitableSwapMove(self, candidateCust, targetRouteIndex,
                                    targetNodeIndex, moveCost,
                                    moveProfit, psm):
        psm.customer = candidateCust
        psm.positionOfTargetRoute = targetRouteIndex
        psm.positionOfTargetNode = targetNodeIndex
        psm.moveCost = moveCost
        psm.moveProfit = moveProfit

    # Apply the results from the FindBestProfitableSwapMove to the final solution
    # We are expecting that the profit will be increased as a new unvisited customer may be swapped with an already
    # visited customer
    def ApplyProfitableSwapMove(self, psm: ProfitableSwapMove, iterator):
        oldCost = self.CalculateTotalCost(self.sol)
        oldProfit = self.CalculateTotalProfit(self.sol)

        rt = self.sol.routes[psm.positionOfTargetRoute]
        b = rt.sequenceOfNodes[psm.positionOfTargetNode]
        psm.customer = b

        rt.duration += psm.moveCost
        rt.profit += psm.moveProfit

        self.sol.cost += psm.moveCost
        self.sol.profit += psm.moveProfit

        newCost = self.CalculateTotalCost(self.sol)
        newProfit = self.CalculateTotalProfit(self.sol)

        self.SetTabuIterator(b, iterator)

        if abs((newCost - oldCost) - psm.moveCost) > 0.0001:
            print('Cost Issue')
            # self.TestSolution()

        if abs((newProfit - oldProfit) + psm.moveProfit) < 0.0001:
            print("Profit Issue")

    def MoveIsTabu(self, n: Node, iterator, moveCost):
        if moveCost + self.sol.cost < self.bestSolution.cost - 0.001:
            return False
        if iterator < n.isTabuTillIterator:
            return True
        return False

    def SetTabuIterator(self, n: Node, iterator):
        n.isTabuTillIterator = iterator + random.randint(self.minTabuTenure, self.maxTabuTenure)

    # Function to calculate the total cost of the solution
    def CalculateTotalCost(self, sol):
        c = 0
        for i in range(0, len(sol.routes)):
            rt = sol.routes[i]
            for j in range(0, len(rt.sequenceOfNodes) - 1):
                a = rt.sequenceOfNodes[j]
                b = rt.sequenceOfNodes[j + 1]
                c += self.distanceMatrix[a.id][b.id] + a.service_time
        return c

    # Function to calculate the total profit of the solution
    def CalculateTotalProfit(self, sol):
        p = 0
        for i in range(0, len(sol.routes)):
            rt = sol.routes[i]
            for j in range(0, len(rt.sequenceOfNodes) - 1):
                a = rt.sequenceOfNodes[j]
                p += a.profit
        return p

    # Function to construct the Variable Neighborhood Descent and check the relocation, swap, insertion and
    # profitable swap operators sequentially, until we reach a better final solution, which reduces the cost and
    # maximizes the profit produced from the initial solution.
    def VND(self):
        self.bestSolution = self.cloneSolution(self.sol)
        VNDIterator = 0
        kmax = 3
        localSearchIterator = 0

        # Create an instance of each operator
        rm = RelocationMove()
        sm = SwapMove()
        uim = UnroutedInsertionMove()
        psm = ProfitableSwapMove()

        k = 0
        draw = True

        while k <= kmax:
            self.InitializeOperators(rm, sm, uim, psm)
            if k == 3:
                self.FindBestRelocationMove(rm, localSearchIterator)
                if rm.originRoutePosition is not None and rm.moveCost < 0:
                    self.ApplyRelocationMove(rm, localSearchIterator)
                    if draw:
                        SolDrawer.draw(VNDIterator, self.sol, self.allNodes)
                    VNDIterator = VNDIterator + 1
                    self.searchTrajectory.append(self.sol.cost)
                    self.searchProfitTrajectory.append(self.sol.profit)
                    k = 0
                else:
                    k += 1
            elif k == 2:
                self.FindBestSwapMove(sm, localSearchIterator)
                if sm.positionOfFirstRoute is not None and sm.moveCost < 0:
                    self.ApplySwapMove(sm, localSearchIterator)
                    if draw:
                        SolDrawer.draw(VNDIterator, self.sol, self.allNodes)
                    VNDIterator = VNDIterator + 1
                    self.searchTrajectory.append(self.sol.cost)
                    self.searchProfitTrajectory.append(self.sol.profit)
                    k = 0
                else:
                    k += 1
            elif k == 1:
                self.FindBestUnroutedInsertionMove(uim, localSearchIterator)
                if uim.customer is not None and uim.moveCost < 0 and uim.moveProfit > 0:
                    self.ApplyUnroutedInsertionMove(uim, localSearchIterator)
                    if draw:
                        SolDrawer.draw(VNDIterator, self.sol, self.allNodes)
                    VNDIterator = VNDIterator + 1
                    self.searchTrajectory.append(self.sol.cost)
                    self.searchProfitTrajectory.append(self.sol.profit)
                    k = 0
                else:
                    k += 1
            elif k == 0:
                self.FindBestProfitableSwapMove(psm, localSearchIterator)
                if psm.customer is not None and psm.moveCost < 0 and psm.moveProfit > 0:
                    self.ApplyProfitableSwapMove(psm, localSearchIterator)
                    if draw:
                        SolDrawer.draw(VNDIterator, self.sol, self.allNodes)
                    VNDIterator = VNDIterator + 1
                    self.searchTrajectory.append(self.sol.cost)
                    self.searchProfitTrajectory.append(self.sol.profit)
                    k = 0
                else:
                    k += 1

            if self.sol.cost < self.bestSolution.cost and self.sol.profit > self.bestSolution.profit:
                self.bestSolution = self.cloneSolution(self.sol)

        # Call Solution Drawer for the figures and plots
        SolDrawer.draw('final_vnd', self.bestSolution, self.allNodes)
        SolDrawer.drawTrajectory(self.searchTrajectory)
        SolDrawer.drawProfitTrajectory(self.searchProfitTrajectory)

    # Function to calculate the solution of Minimum Insertions algorithm
    def TestSolution(self):
        totalSolCost = 0
        for c in range(0, len(self.customers)):
            cust: Node = self.customers[c]
            for r in range(0, len(self.sol.routes)):
                rt: Route = self.sol.routes[r]
                rtCost = 0
                rtProfit = 0
                rtNodeId = None
                for n in range(0, len(rt.sequenceOfNodes) - 1):
                    A = rt.sequenceOfNodes[n]
                    B = rt.sequenceOfNodes[n + 1]
                    rtCost += self.distanceMatrix[A.id][cust.id] + self.distanceMatrix[cust.id][B.id] + \
                              cust.service_time
                    rtProfit += cust.profit
                    rtNodeId = rt.sequenceOfNodes[n].id
                if abs(rtCost - rt.duration) > 0.0001:
                    # commented to avoid long output
                    # print('Route Cost problem')
                    continue

                totalSolCost += rt.duration

            if abs(totalSolCost - self.sol.cost) > 0.0001:
                # commented to avoid long output
                # print('Solution Cost problem')
                continue
                