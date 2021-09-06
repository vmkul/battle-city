import pygame as pg
from util import *
from explosion import *

explosion_sound = load_sound("samples/explosion.wav")


class Bullet(pg.sprite.Sprite):
    def __init__(self, pos, direction, game, is_enemy):
        pg.sprite.Sprite.__init__(self)
        self.image, self.rect = load_image("sprites/bullet.png", -1)
        self.rect.center = pos
        self.game = game
        self.is_enemy = is_enemy

        if direction == "UP":
            self.speed = 0, -4
            self.image = pg.transform.rotate(self.image, -90)
        elif direction == "DOWN":
            self.image = pg.transform.rotate(self.image, 90)
            self.speed = 0, 4
        elif direction == "LEFT":
            self.speed = -4, 0
        elif direction == "RIGHT":
            self.image = pg.transform.rotate(self.image, 180)
            self.speed = 4, 0

    def destroy_tank(self, tank):
        explosion_sound.play()
        self.game.create_explosion(tank.rect.topleft)
        tank.destroy()
        self.kill()

    def update(self):
        newpos = self.rect.move(self.speed)
        test_rect = newpos.inflate(-15, -15)

        if self.game.collides_with_map(test_rect):
            return self.kill()

        wall = self.game.get_wall_collision(test_rect)
        if wall is not None:
            self.game.create_explosion(wall.rect.topleft)
            explosion_sound.play()
            wall.kill()
            return self.kill()

        if self.is_enemy:
            target_tank = self.game.get_player_tank_collision(test_rect)
            if target_tank is not None:
                return self.destroy_tank(target_tank)
        else:
            enemies = self.game.get_enemy_tank_collision(test_rect)
            if len(enemies) > 0:
                self.destroy_tank(enemies[0])

        base_collision = self.game.get_base_collision(test_rect)
        if base_collision is not None:
            self.game.create_explosion(base_collision.rect.topleft)
            self.kill()
            base_collision.destroy()

        self.rect = newpos
