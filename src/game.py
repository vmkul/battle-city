import os
import random
import math
from datetime import datetime
import pygame as pg
from wall import *
from tank import *
from bullet import *
from explosion import *
from base import *
from score import *
from search import *
from path_symbol import *
from profiler import *
from util import *
from logger import Logger
from minimax import minimax, expectimax
import numpy as np
import time


random.seed()
RESTRICTED_TILES = [(12, 5), (12, 6), (12, 7), (11, 5),
                    (11, 6), (11, 7), (10, 6)]
SPAWN_ENEMY_EVENT = pg.USEREVENT
ENEMY_COUNT = 1
RANDOM_ENEMY_COUNT = 0
MAX_ENEMY_COUNT = 4
GAME_STATE_ACTIVE = 0
GAME_STATE_PLAYER_WON = 1
GAME_STATE_PLAYER_LOST = 2
SEARCH_ALGORITHMS = [BFS, DFS, UCS, A_Star]

win_sound = load_sound("samples/win_effect.wav")
lose_sound = load_sound("samples/lose_effect.wav")


class Game:
    def __init__(self, graphical_mode=True, player_algorithm=minimax):
        pg.init()
        self.graphical_mode = graphical_mode

        screen = pg.Surface((480, 426))
        if graphical_mode:
            screen = pg.display.set_mode((480, 426), flags=pg.SCALED)

        self.screen = screen
        pg.display.set_caption("Battle City")

        background = pg.Surface(screen.get_size())
    
        if graphical_mode:
            background = background.convert()

        background.fill((128, 128, 128))
        self.background = background

        self.game_map = pg.Surface((416, 416))
        if graphical_mode:
            self.game_map = self.game_map.convert()
        self.game_map.fill((0, 0, 0))
        self.game_map_rect = self.game_map.get_rect()

        background.blit(self.game_map, MAP_COORDINATES)
        screen.blit(background, (0, 0))

        self.bullet_sprites = pg.sprite.RenderPlain()
        self.base_sprite = pg.sprite.RenderPlain()
        self.wall_sprites = pg.sprite.RenderPlain()
        self.player_tank_sprites = pg.sprite.RenderPlain()
        self.enemy_tank_sprites = pg.sprite.RenderPlain()
        self.explosion_sprites = pg.sprite.RenderPlain()
        self.enemy_counter_sprite = pg.sprite.RenderPlain(EnemyCounter(10))
        self.path_symbol_sprites = pg.sprite.RenderPlain()

        self.map_coords = []
        self.player_tank = PlayerTank(10, 6, self, player_algorithm)
        self.player_algorithm = player_algorithm
        self.enemy_count = ENEMY_COUNT + RANDOM_ENEMY_COUNT
        self.enemies = []
        self.game_state = GAME_STATE_ACTIVE
        self.player_move = True
        self.search_algorithm_profiler = Profiler(BFS)
        self.logger = Logger()
        self.game_start_time = datetime.now()
        self.search_algorithm = self.search_algorithm_profiler.execute

        self.game_over_font = pg.font.Font(
            data_dir + "/fonts/ARCADECLASSIC.TTF", 36)
        self.game_over_text = self.game_over_font.render(
            "YOU   LOST!", 1, (0, 255, 0))
        self.textpos = self.game_over_text.get_rect(
            center=(self.game_map.get_width() / 2, self.game_map.get_height() / 2))

        self.restart_game()

    def restart_game(self):
        self.game_start_time = datetime.now()
        self.game_state = GAME_STATE_ACTIVE
        self.player_move = True

        self.bullet_sprites.empty()
        self.base_sprite.empty()
        self.wall_sprites.empty()
        self.enemy_tank_sprites.empty()
        self.explosion_sprites.empty()
        self.player_tank_sprites.empty()

        self.wall_sprites.add(
            Wall(12, 5),
            Wall(12, 7),
            Wall(11, 5),
            Wall(11, 6),
            Wall(11, 7),
        )
        self.base_sprite.add(Base())

        self.map_coords.clear()

        for i in range(13):
            for j in range(13):
                if (i, j) not in RESTRICTED_TILES:

                    self.map_coords.append((i, j))

        for i in range(40):
            coord = random.choice(self.map_coords)
            self.map_coords.remove(coord)
            self.wall_sprites.add(Wall(coord[0], coord[1]))

        for i in range(ENEMY_COUNT):
            self.enemies.append(AITank)

        for i in range(RANDOM_ENEMY_COUNT):
            self.enemies.append(RandomTank)

        random.shuffle(self.enemies)

        self.enemy_count = ENEMY_COUNT + RANDOM_ENEMY_COUNT
        for i in range(MAX_ENEMY_COUNT):
            if self.enemy_count > 0:
                self.enemy_count = self.enemy_count - 1
                self.spawn_enemy()

        self.player_tank = PlayerTank(10, 6, self, self.player_algorithm)
        self.player_tank_sprites.add(self.player_tank)

    def get_wall_collision(self, rect):
        for wall in self.wall_sprites:
            if wall.rect.colliderect(rect):
                return wall
        return None

    def get_enemy_tank_collision(self, rect):
        res = []
        for tank in self.enemy_tank_sprites:
            if tank.rect.colliderect(rect):
                res.append(tank)
        return res

    def get_player_tank_collision(self, rect):
        for tank in self.player_tank_sprites:
            if tank.rect.colliderect(rect):
                return tank
        return None

    def get_base_collision(self, rect):
        for base in self.base_sprite:
            if base.rect.colliderect(rect):
                return base
        return None

    def collides_with_map(self, rect):
        return not self.game_map_rect.contains(rect)

    def spawn_enemy(self):
        rows = [n for n in range(13)]
        cols = [n for n in range(13)]

        random.shuffle(rows)
        random.shuffle(cols)

        for i in rows:
            for j in cols:
                if (i, j) in RESTRICTED_TILES:
                    continue
                test_rect = pg.Rect(
                    j * 32 + MAP_COORDINATES[0], i * 32 + MAP_COORDINATES[1], 32, 32)

                if self.get_wall_collision(test_rect) is not None or len(self.get_enemy_tank_collision(
                        test_rect)) > 0 or self.get_player_tank_collision(test_rect) is not None:
                    continue

                return self.enemy_tank_sprites.add(self.enemies.pop()(i, j, self))

    def get_square_matrix(self, omit=[]):
        res = [[0 for x in range(13)] for y in range(13)]

        for i in range(13):
            for j in range(13):
                test_rect = pg.Rect(
                    j * 32 + MAP_COORDINATES[0], i * 32 + MAP_COORDINATES[1], 16, 16)
                if (i, j) in omit or (self.get_wall_collision(test_rect) is None and len(self.get_enemy_tank_collision(test_rect)) == 0):
                    res[i][j] = 1

        return res

    def get_random_tank_sprites(self):
        res = []

        for tank in self.enemy_tank_sprites:
            if isinstance(tank, RandomTank):
                res.append(tank)

        return res

    def generate_path_to_player(self, enemy_tank):
        player = self.player_tank.get_current_tile()
        enemy_tank_tile = enemy_tank.get_current_tile()
        m = self.get_square_matrix([enemy_tank_tile])

        res = self.search_algorithm(m, player, enemy_tank_tile)
        res.reverse()

        return res

    def draw_paths_to_enemies(self):
        self.path_symbol_sprites.empty()
        paths = []

        for tank in self.enemy_tank_sprites:
            paths.append(self.generate_path_to_player(tank))

        for path in paths:
            for square in path:
                self.path_symbol_sprites.add(PathSymbol(square[0], square[1]))

    def switch_search_algorithm(self):
        index = SEARCH_ALGORITHMS.index(
            self.search_algorithm_profiler.get_func())
        index = index + 1 if index < len(SEARCH_ALGORITHMS) - 1 else 0
        self.search_algorithm_profiler.set_func(SEARCH_ALGORITHMS[index])

    def create_explosion(self, pos):
        self.explosion_sprites.add(Explosion(pos[0], pos[1]))

    def print_search_algorithm(self):
        font = pg.font.Font(data_dir + "/fonts/ARCADECLASSIC.TTF", 26)
        font2 = pg.font.Font(data_dir + "/fonts/INVASION2000.TTF", 20)

        algorithm_name = font.render(
            self.search_algorithm_profiler.get_func_name(), 1, (34, 34, 34))
        algorithm_duration = font2.render("{:.2f}".format(
            self.search_algorithm_profiler.get_avg_duration()), 1, (34, 34, 34))

        self.background.blit(
            algorithm_name, (480 - algorithm_name.get_rect().width, 0))
        self.background.blit(
            algorithm_duration,
            (480 - algorithm_duration.get_rect().width,
             algorithm_name.get_height()))

    def print_win(self):
        self.game_over_text = self.game_over_font.render(
            "YOU   WON!", 1, (0, 255, 0))
        self.game_map.blit(self.game_over_text, self.textpos)

    def print_lose(self):
        self.game_over_text = self.game_over_font.render(
            "YOU   LOST!", 1, (255, 0, 0))
        self.game_map.blit(self.game_over_text, self.textpos)

    def get_score(self):
        return self.player_tank.lives * 0.2 - \
            (self.enemy_count + len(self.enemy_tank_sprites)) * 0.1

    def log_game_result(self):
        status = "player_won" if self.game_state == GAME_STATE_PLAYER_WON else "player_lost"
        duration = (datetime.now() - self.game_start_time).total_seconds()

        self.logger.log_result(status, duration,
                               self.get_score(), self.player_algorithm.__name__)

    def check_or_update_game_state(self):
        if self.game_state == GAME_STATE_ACTIVE:
            if not self.player_tank.is_alive:
                lose_sound.play()
                self.game_state = GAME_STATE_PLAYER_LOST
                self.log_game_result()

            if self.enemy_count == 0 and len(self.enemy_tank_sprites) == 0:
                win_sound.play()
                self.game_state = GAME_STATE_PLAYER_WON
                self.log_game_result()

    def update_game_state(self):
        self.check_or_update_game_state()
        self.wall_sprites.update()
        self.bullet_sprites.update()
        self.base_sprite.update()
        self.explosion_sprites.update()
        self.enemy_counter_sprite.sprites()[0].set_count(self.player_tank.lives)
        self.enemy_counter_sprite.update()
        self.path_symbol_sprites.update()

    def update_game_map(self):
        self.game_map.fill((0, 0, 0))
        self.path_symbol_sprites.draw(self.game_map)
        self.wall_sprites.draw(self.game_map)
        self.bullet_sprites.draw(self.game_map)
        self.enemy_tank_sprites.draw(self.game_map)
        self.player_tank_sprites.draw(self.game_map)
        self.base_sprite.draw(self.game_map)
        self.explosion_sprites.draw(self.game_map)

    def toggle_player_move(self):
        self.player_move = not self.player_move
    
    def render_map(self):
        arr = np.array(self.game_map.get_buffer())
        arr = np.reshape(arr, (416, 416, 4))
        arr = arr[:, :, 0:3]

        return arr

    def step(self, move):
        possible_moves = [
            self.player_tank.move_up,
            self.player_tank.move_down,
            self.player_tank.move_left,
            self.player_tank.move_right,
            self.player_tank.stop]

        self.player_tank.shoot()
        possible_moves[move]()
        self.player_tank.move()
        self.update_game_state()

        self.enemy_tank_sprites.update()
        self.update_game_state()
        self.update_game_map()

        return (self.get_score(), self.game_state != GAME_STATE_ACTIVE)

        

    def main_loop(self):
        if self.graphical_mode:
            pg.display.flip()
        clock = pg.time.Clock()
        spawn_pending = False

        going = True

        while going:
            self.draw_paths_to_enemies()
            self.check_or_update_game_state()

            if len(self.enemy_tank_sprites) < MAX_ENEMY_COUNT and not spawn_pending:
                pg.time.set_timer(SPAWN_ENEMY_EVENT, 2000)
                spawn_pending = True

            for event in pg.event.get():
                if event.type == pg.QUIT:
                    going = False

                elif event.type == pg.KEYDOWN:
                    if event.key == pg.K_z:
                        self.switch_search_algorithm()

                elif event.type == SPAWN_ENEMY_EVENT:
                    if self.enemy_count != 0:
                        self.enemy_count = self.enemy_count - 1
                        pg.time.set_timer(SPAWN_ENEMY_EVENT, 0)
                        self.spawn_enemy()
                        spawn_pending = False

            if self.player_move:
                self.player_tank_sprites.update()
                self.update_game_state()
                self.toggle_player_move()
            else:
                self.enemy_tank_sprites.update()
                self.update_game_state()
                self.toggle_player_move()

            self.background.fill((128, 128, 128))

            self.update_game_map()
            self.enemy_counter_sprite.draw(self.background)
            self.print_search_algorithm()

            if self.game_state == GAME_STATE_PLAYER_WON:
                self.restart_game()
                self.print_win()
            elif self.game_state == GAME_STATE_PLAYER_LOST:
                self.restart_game()
                self.print_lose()

            self.background.blit(self.game_map, MAP_COORDINATES)
            self.screen.blit(self.background, (0, 0))

            if self.graphical_mode:
                pg.display.flip()
            time.sleep(1)

        pg.quit()


if __name__ == "__main__":
    game = Game()
    game.main_loop()
