class GameMap():
    def __init__(self, square_matrix):
        self.square_matrix = square_matrix

    def get_free_adjacent_vertices(self, vertex):
        i, j = vertex.coord
        res = []

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


def generate_full_path(vertex):
    res = []

    while vertex is not None:
        res.append(vertex.coord)
        vertex = vertex.prev

    return res
