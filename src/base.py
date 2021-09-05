import os
import pygame as pg
from util import *

BASE_ANIM_PATHS = os.listdir(os.path.join(data_dir, "sprites/base_anim"))
BASE_ANIM_PATHS.sort()


class Base(pg.sprite.Sprite):
    def __init__(self):
        pg.sprite.Sprite.__init__(self)
        self.base_anim = [load_image("sprites/base_anim/" + path)
                          for path in BASE_ANIM_PATHS]
        self.dead_image = load_image("sprites/dead_base.png")[0]
        self.dead_image = pg.transform.scale(self.dead_image, (32, 32))
        self.image, self.rect = self.base_anim[0]

        self.image = pg.transform.scale(self.image, (32, 32))
        self.rect = self.rect.inflate(-20, -20)
        self.rect.topleft = 6 * 32, 12 * 32

        self.sprite_index = 0
        self.draw = False
        self.is_alive = True

    def destroy(self):
        self.is_alive = False

    def update(self):
        if not self.draw:
            self.draw = True
            return
        else:
            self.draw = False

        if self.is_alive:
            self.sprite_index = 0 if self.sprite_index == 17 else self.sprite_index + 1
            self.image = pg.transform.scale(
                self.base_anim[self.sprite_index][0], (32, 32))
        else:
            self.image = self.dead_image
