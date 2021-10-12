import math
import functools


class GameMap():
    def __init__(self, square_matrix):
        self.square_matrix = square_matrix

    def get_free_adjacent_vertices(self, vertex):
        i, j = vertex.coord
        res = []

        if i not in range(13) or j not in range(13):
            return []

        if i < 12 and self.square_matrix[i + 1][j] == 1:
            res.append(Vertex(vertex, (i + 1, j)))
        if i > 0 and self.square_matrix[i - 1][j] == 1:
            res.append(Vertex(vertex, (i - 1, j)))
        if j < 12 and self.square_matrix[i][j + 1] == 1:
            res.append(Vertex(vertex, (i, j + 1)))
        if j > 0 and self.square_matrix[i][j - 1] == 1:
            res.append(Vertex(vertex, (i, j - 1)))

        return res


class Vertex():
    def __init__(self, prev, coord):
        self.prev = prev
        self.coord = coord


def BFS(game_map, root, goal):
    graph = GameMap(game_map)
    queue = [Vertex(None, root)]
    explored = [root]

    while len(queue) != 0:
        v = queue.pop(0)

        if v.coord == goal:
            return generate_full_path(v)

        for adj_v in graph.get_free_adjacent_vertices(v):
            if adj_v.coord not in explored:
                explored.append(adj_v.coord)
                queue.append(adj_v)

    return []


def DFS(game_map, root, goal):
    graph = GameMap(game_map)
    stack = [Vertex(None, root)]
    explored = []

    while len(stack) != 0:
        v = stack.pop()

        if v.coord == goal:
            return generate_full_path(v)

        if v.coord not in explored:
            explored.append(v.coord)
            for adj_v in graph.get_free_adjacent_vertices(v):
                stack.append(adj_v)

    return []


def UCS(game_map, root, goal):
    graph = GameMap(game_map)
    cost = {root: 0}
    path = {root: [root]}
    priority_queue = [root]

    while len(priority_queue) != 0:
        current = priority_queue.pop(0)

        for adj_v in graph.get_free_adjacent_vertices(Vertex(None, current)):
            adj_v = adj_v.coord
            if adj_v not in cost:
                cost[adj_v] = math.inf

            if cost[adj_v] > cost[current] + 1:
                priority_queue.append(adj_v)
                cost[adj_v] = cost[current] + 1
                path[adj_v] = path[current].copy()
                path[adj_v].append(adj_v)

    if goal in path:
        return path[goal]
    else:
        return []


def generate_full_path(vertex):
    res = []

    while vertex is not None:
        res.insert(0, vertex.coord)
        vertex = vertex.prev

    return res


def manhattan_distance(root, goal):
    return abs(root[0] - goal[0]) + abs(root[1] - goal[1])


def reconstruct_path(came_from, current):
    total_path = [current]

    while True:
        if current not in came_from:
            break
        current = came_from[current]
        total_path.insert(0, current)

    return total_path


def A_Star(game_map, root, goal):
    graph = GameMap(game_map)
    open_set = [root]
    came_from = {}
    g_score = {root: 0}
    f_score = {root: 0}

    while len(open_set) > 0:
        current = functools.reduce(
            lambda a, b: a if f_score[a] < f_score[b] else b, open_set)

        if current == goal:
            return reconstruct_path(came_from, current)

        open_set.remove(current)

        for neighbor in graph.get_free_adjacent_vertices(
                Vertex(None, current)):
            neighbor = neighbor.coord
            tentative_g_score = g_score[current] + 1
            if tentative_g_score < g_score.get(neighbor, math.inf):
                came_from[neighbor] = current
                g_score[neighbor] = tentative_g_score
                f_score[neighbor] = g_score[neighbor] + \
                    manhattan_distance(neighbor, goal)
                if neighbor not in open_set:
                    open_set.append(neighbor)

    return []
