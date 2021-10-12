import pygame as pg
from bullet import *
from util import *
import random
import math
from search import A_Star
from minimax import minimax

motor_sound = load_sound("samples/motor.wav")
shot_sound = load_sound("samples/gun_shot.wav")
motor_sound.set_volume(0.5)
shot_sound.set_volume(0.5)

TANK_SPEED = 32


class Tank(pg.sprite.Sprite):
    def __init__(self, row, col, game, sprite="sprites/tank.bmp"):
        pg.sprite.Sprite.__init__(self)
        self.image, self.rect = load_image(sprite, -1)
        self.rect.topleft = col * 32, row * 32
        self.original = self.image
        self.speed = 0, 0
        self.game = game
        self.direction = "UP"
        self.is_enemy = isinstance(self, AITank)
        self.is_alive = True
        self.last_shot_time = pg.time.get_ticks()
        self.lives = 1

    def move_right(self):
        self.image = pg.transform.rotate(self.original, -90)
        self.direction = "RIGHT"
        self.speed = TANK_SPEED, 0

    def move_left(self):
        self.image = pg.transform.rotate(self.original, 90)
        self.direction = "LEFT"
        self.speed = -TANK_SPEED, 0

    def move_up(self):
        self.image = self.original
        self.direction = "UP"
        self.speed = 0, -TANK_SPEED

    def move_down(self):
        self.image = pg.transform.rotate(self.original, 180)
        self.direction = "DOWN"
        self.speed = 0, TANK_SPEED

    def stop(self):
        self.is_playing_motor_sound = False
        motor_sound.stop()
        self.speed = 0, 0

    def is_colliding(self, newpos, omit_player=False):
        enemy_tank_collisions = self.game.get_enemy_tank_collision(newpos)

        if len(enemy_tank_collisions) > 0:
            return True

        if self.game.collides_with_map(newpos):
            return True

        if self.game.get_wall_collision(newpos) is not None:
            return True

        if self.is_enemy:
            if self.game.get_player_tank_collision(
                    newpos) is not None and not omit_player:
                return True

        return True if self.game.get_base_collision(
            newpos) is not None else False

    def get_available_move_directions(self):
        possible_directions = []

        if not self.is_colliding(self.rect.move((0, -TANK_SPEED))):
            possible_directions.append(self.move_up)
        if not self.is_colliding(self.rect.move(0, TANK_SPEED)):
            possible_directions.append(self.move_down)
        if not self.is_colliding(self.rect.move(TANK_SPEED, 0)):
            possible_directions.append(self.move_right)
        if not self.is_colliding(self.rect.move(-TANK_SPEED, 0)):
            possible_directions.append(self.move_left)

        return possible_directions

    def destroy(self):
        self.lives = self.lives - 1
        if self.lives <= 0:
            self.is_alive = False
            self.kill()

    def shoot(self):
        now = pg.time.get_ticks()
        if not self.is_alive:
            return

        self.last_shot_time = now
        coord = self.rect.center
        if self.direction == "UP" or self.direction == "DOWN":
            coord = coord[0] + 2, coord[1]

        shot_sound.stop()
        shot_sound.play()
        self.game.bullet_sprites.add(
            Bullet(coord, self.direction, self.game, self.is_enemy))

    def has_enemy_ahead(self):
        i, j = self.get_current_tile()
        targets = self.game.player_tank_sprites if self.is_enemy else self.game.enemy_tank_sprites
        tiles_ahead = []

        if self.direction == "UP":
            def move(x): return (x[0] - 1, x[1])
        elif self.direction == "DOWN":
            def move(x): return (x[0] + 1, x[1])
        elif self.direction == "LEFT":
            def move(x): return (x[0], x[1] - 1)
        elif self.direction == "RIGHT":
            def move(x): return (x[0], x[1] + 1)

        while i in range(13) and j in range(13):
            i, j = move((i, j))
            tiles_ahead.append((i, j))

        for target in targets:
            if target.get_current_tile() in tiles_ahead:
                return True

        return False

    def get_current_tile(self):
        center = self.rect.center
        i = math.floor(center[1] / 32)
        j = math.floor(center[0] / 32)

        return (i, j)

    def is_in_center(self):
        i, j = self.get_current_tile()
        tile_topleft = (j * 32, i * 32)

        return tile_topleft == self.rect.topleft

    def traverse_path(self, path):
        if not self.is_in_center():
            return
        if len(path) < 2:
            return

        start = path[0]
        dest = path[1]
        up_down = dest[0] - start[0]
        left_right = dest[1] - start[1]

        if up_down != 0:
            if up_down > 0:
                self.move_down()
            else:
                self.move_up()

        if left_right != 0:
            if left_right > 0:
                self.move_right()
            else:
                self.move_left()

    def move_to_opposite_direction(self):
        if self.direction == "UP":
            self.move_down()
        elif self.direction == "DOWN":
            self.move_up()
            pass
        elif self.direction == "RIGHT":
            self.move_left()
        else:
            self.move_right()


class PlayerTank(Tank):
    def __init__(self, x, y, game):
        Tank.__init__(self, x, y, game)
        self.is_playing_motor_sound = False
        self.dest_tile = None
        self.lives = 10

    def choose_free_tile(self):
        free_tiles = []
        m = self.game.get_square_matrix()

        for i in range(len(m)):
            for j in range(len(m[0])):
                if m[i][j] == 1:
                    free_tiles.append((i, j))

        return random.choice(free_tiles)

    def move(self):
        newpos = self.rect.move(self.speed)
        if not self.is_colliding(newpos):
            self.rect = newpos

    def update(self):
        self.shoot()

        func = minimax(self.game)
        getattr(self, func.__name__)()

        if self.speed != (0, 0) and not self.is_playing_motor_sound:
            self.is_playing_motor_sound = True
            motor_sound.play(loops=-1)

        self.move()


class AITank(Tank):
    def __init__(self, x, y, game):
        Tank.__init__(self, x, y, game, "sprites/enemy_tank.bmp")
        self.is_moving = False

    def move_to_free_location(self):
        possible_directions = self.get_available_move_directions()

        if len(possible_directions) > 0:
            self.is_moving = True
            random.choice(possible_directions)()

    def update(self):
        path = self.game.generate_path_to_player(self)
        self.traverse_path(path)

        if len(path) == 0:
            self.shoot()

        if self.has_enemy_ahead():
            self.shoot()

        newpos = self.rect.move(self.speed)

        if self.is_colliding(newpos, True):
            self.move_to_opposite_direction()

        if self.is_colliding(newpos):
            self.is_moving = False
        else:
            self.rect = newpos
