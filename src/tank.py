import pygame as pg
from bullet import *
from util import *
import random

motor_sound = load_sound("samples/motor.wav")
motor_sound.set_volume(0.3)
shot_sound = load_sound("samples/gun_shot.wav")


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

    def move_right(self):
        self.image = pg.transform.rotate(self.original, -90)
        self.direction = "RIGHT"
        self.speed = 2, 0

    def move_left(self):
        self.image = pg.transform.rotate(self.original, 90)
        self.direction = "LEFT"
        self.speed = -2, 0

    def move_up(self):
        self.image = self.original
        self.direction = "UP"
        self.speed = 0, -2

    def move_down(self):
        self.image = pg.transform.rotate(self.original, 180)
        self.direction = "DOWN"
        self.speed = 0, 2

    def stop(self):
        motor_sound.stop()
        self.speed = 0, 0

    def is_colliding(self, newpos):
        enemy_tank_collisions = self.game.get_enemy_tank_collision(newpos)
        if self.is_enemy:
            enemy_tank_collisions.remove(self)

        if len(enemy_tank_collisions) > 0:
            return True

        if self.game.collides_with_map(newpos):
            return True

        if self.game.get_wall_collision(newpos) is not None:
            return True

        if self.is_enemy:
            if self.game.get_player_tank_collision(newpos) is not None:
                return True

        return True if self.game.get_base_collision(newpos) is not None else False

    def shoot(self):
        coord = self.rect.center
        if self.direction == "UP" or self.direction == "DOWN":
            coord = coord[0] + 8, coord[1]
        shot_sound.stop()
        shot_sound.play()
        self.game.bullet_sprites.add(
            Bullet(coord, self.direction, self.game, self.is_enemy))

    def update(self):
        if self.speed != (0, 0):
            motor_sound.play()
        newpos = self.rect.move(self.speed)
        if not self.is_colliding(newpos):
            self.rect = newpos


class AITank(Tank):
    def __init__(self, x, y, game):
        Tank.__init__(self, x, y, game, "sprites/enemy_tank.jpg")
        self.is_moving = False
        self.last_shot_time = pg.time.get_ticks()

    def move_to_free_location(self):
        possible_directions = []

        if not self.is_colliding(self.rect.move((0, -2))):
            possible_directions.append(self.move_up)
        if not self.is_colliding(self.rect.move(0, 2)):
            possible_directions.append(self.move_down)
        if not self.is_colliding(self.rect.move(2, 0)):
            possible_directions.append(self.move_right)
        if not self.is_colliding(self.rect.move(-2, 0)):
            possible_directions.append(self.move_left)

        if len(possible_directions) > 0:
            self.is_moving = True
            random.choice(possible_directions)()

    def update(self):
        if not self.is_moving:
            self.move_to_free_location()

        newpos = self.rect.move(self.speed)

        current_time = pg.time.get_ticks()
        if current_time - self.last_shot_time >= 1000:
            self.shoot()
            self.last_shot_time = current_time

        if self.is_colliding(newpos):
            self.is_moving = False
        else:
            self.rect = newpos
