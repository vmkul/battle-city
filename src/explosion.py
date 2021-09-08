import pygame as pg
from util import *

explosion_sound = load_sound("samples/explosion.wav")
explosion_sound.set_volume(0.3)


class Explosion(pg.sprite.Sprite):
    def __init__(self, x, y):
        pg.sprite.Sprite.__init__(self)
        self.original, self.rect = load_image("sprites/explosion.png", -1)
        self.rect.topleft = x, y
        self.original = pg.transform.scale(self.original, (384, 32))
        self.image = self.original.subsurface(pg.Rect(32 * 0, 0, 32, 32))

        self.frame = 0
        self.draw = False

    def update(self):
        if not self.draw:
            self.draw = True
            return
        else:
            self.draw = False

        if self.frame == 0:
            explosion_sound.play()

        self.frame = self.frame + 1
        if self.frame == 12:
            return self.kill()

        self.image = self.original.subsurface(
            pg.Rect(32 * self.frame, 0, 32, 32))
