import pygame as pg
from util import *

explosion_sound = load_sound("samples/explosion.wav")

class Bullet(pg.sprite.Sprite):
    def __init__(self, pos, direction, game, tank):
        pg.sprite.Sprite.__init__(self)
        self.image, self.rect = load_image("sprites/bullet.png", -1)
        self.rect.center = pos
        self.game = game
        self.tank = tank

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

    def does_collide(self, rect):
        return self.rect.colliderect(rect)

    def update(self):
        newpos = self.rect.move(self.speed)

        if not self.game.game_map_rect.contains(newpos.inflate(-5, -5)):
            self.kill()
            return

        for wall in self.game.wall_sprites:
            if wall.does_collide(newpos.inflate(-15, -15)):
                explosion_sound.play()
                wall.kill()
                self.kill()
                return

        for tank in self.game.tank_sprites:
            if tank.rect.colliderect(newpos.inflate(-15, -15)) and tank != self.tank:
                explosion_sound.play()
                self.kill()
                tank.kill()
                return

        self.rect = newpos
