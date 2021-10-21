from deepcopy import deepcopy
from game import GAME_STATE_PLAYER_WON, GAME_STATE_ACTIVE
import math


TREE_DEPTH = 4
EXPECTIMAX_DEPTH = 4
MAX_PLAYER = 0
MIN_PLAYER = 1


class Node:
    def __init__(self, game, parent, move, rec=0, player=MAX_PLAYER):
        self.game = game
        self.player = player
        self.score = None
        self.parent = parent
        self.rec = rec
        self.move = move
        self.best_move = self.game.player_tank.stop
        self.alpha = -math.inf
        self.beta = math.inf
        self.expand()

    def eval(self, score, move):
        if self.score is None:
            self.best_move = move
            self.score = score
            if self.player == MIN_PLAYER:
                self.beta = score
            return self.eval_parent()

        if self.player == MAX_PLAYER:
            if score > self.score:
                self.score = score
                self.best_move = move
                self.eval_parent()
        else:
            if score < self.score:
                self.beta = score
                self.score = score
                self.best_move = move
                self.eval_parent()

    def eval_parent(self):
        if self.parent is not None:
            self.parent.eval(self.score, self.move)

    def min_move(self):
        self.game.enemy_tank_sprites.update()
        self.game.update_game_state()
        Node(deepcopy(self.game), self, self.move, self.rec + 1)

    def max_move(self):
        possible_directions = [
            self.game.player_tank.move_up,
            self.game.player_tank.move_down,
            self.game.player_tank.move_left,
            self.game.player_tank.move_right,
            self.game.player_tank.stop]

        for direction in possible_directions:
            game = deepcopy(self.game)
            player_tank = game.player_tank

            player_tank.shoot()
            getattr(player_tank, direction.__name__)()
            player_tank.move()
            game.update_game_state()

            Node(game, self, direction, self.rec + 1, MIN_PLAYER)

            if self.parent is not None and self.parent.alpha > self.beta:
                return

        self.alpha = self.score

    def expand(self):
        if self.game.game_state != GAME_STATE_ACTIVE:
            if self.parent is None:
                return
            if self.game.game_state == GAME_STATE_PLAYER_WON:
                return self.parent.eval(1, self.move)
            else:
                return self.parent.eval(-1, self.move)

        if self.rec > TREE_DEPTH:
            score = self.game.player_tank.lives * 0.2 - \
                (self.game.enemy_count + len(self.game.enemy_tank_sprites)) * 0.1
            return self.parent.eval(score, self.move)

        if self.player == MIN_PLAYER:
            self.min_move()
        else:
            self.max_move()

    def get_next_move(self):
        return self.best_move.__name__


class ExpectiNode:
    def __init__(self, game, rec=0):
        self.game = game
        self.score = -math.inf
        self.rec = rec
        self.children = {}
        self.best_move = "stop"
        self.expand()

    def max_move(self):
        possible_directions = [
            "move_up",
            "move_down",
            "move_left",
            "move_right",
            "stop"]

        for direction in possible_directions:
            game = deepcopy(self.game)
            player_tank = game.player_tank

            player_tank.shoot()
            getattr(player_tank, direction)()
            player_tank.move()

            self.children[direction] = ChanceNode(game, self.rec + 1)

        for k, v in self.children.items():
            if v.score > self.score:
                self.best_move = k
                self.score = v.score

    def expand(self):
        if self.game.game_state != GAME_STATE_ACTIVE:
            if self.game.game_state == GAME_STATE_PLAYER_WON:
                self.score = math.inf
                return
            else:
                self.score = -math.inf
                return

        if self.rec > EXPECTIMAX_DEPTH:
            self.score = self.game.player_tank.lives * 0.2 - \
                (self.game.enemy_count + len(self.game.enemy_tank_sprites)) * 0.1
            return

        self.max_move()

    def get_next_move(self):
        return self.best_move


class ChanceNode:
    def __init__(self, game, rec=0):
        self.game = game
        self.score = 0
        self.rec = rec
        self.children = []
        self.expand()

    def random_move(self, index=0):
        random_tanks = self.game.get_random_tank_sprites()
        if index > len(random_tanks) - 1:
            return

        directions = random_tanks[index].get_available_move_directions()
        if len(directions) == 0:
            directions.append(random_tanks[index].stop)

        for direction in directions:
            game = deepcopy(self.game)
            t = game.get_random_tank_sprites()[index]
            getattr(t, direction.__name__)()

            t.shoot_on_sight()
            game.update_game_state()

            self.children.append(ExpectiNode(game, self.rec + 1))

        self.random_move(index + 1)

    def expand(self):
        random_tanks = self.game.get_random_tank_sprites()
        for tank in self.game.enemy_tank_sprites:
            if tank not in random_tanks:
                tank.update()

        if len(random_tanks) == 0:
            self.game.update_game_state()
            self.children.append(ExpectiNode(
                deepcopy(self.game), self.rec + 1))
        else:
            self.random_move()

        if len(self.children) == 0:
            return

        probability = 1 / len(self.children)

        for child in self.children:
            self.score = self.score + child.score * probability


def minimax(game):
    node = Node(deepcopy(game), None, None)
    return node.get_next_move()


def expectimax(game):
    node = ExpectiNode(deepcopy(game))
    return node.get_next_move()
