import pygame as pg
from util import *


class Wall(pg.sprite.Sprite):
    def __init__(self, row, col):
        pg.sprite.Sprite.__init__(self)
        self.image, self.rect = load_image("sprites/brick_wall.png", -1)
        self.rect.topleft = col * 32, row * 32

    def update(self):
        pass
