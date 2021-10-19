from deepcopy import deepcopy
from game import GAME_STATE_PLAYER_WON, GAME_STATE_ACTIVE
import math


TREE_DEPTH = 4
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
        return self.best_move


def minimax(game):
    node = Node(deepcopy(game), None, None)
    return node.get_next_move()
