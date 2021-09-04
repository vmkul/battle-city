import pygame as pg
from util import *

class Wall(pg.sprite.Sprite):
    def __init__(self, row, col):
        pg.sprite.Sprite.__init__(self)
        self.image, self.rect = load_image("sprites/brick_wall.png", -1)
        self.rect.topleft = col * 32 + MAP_COORDINATES[0], row * 32 + MAP_COORDINATES[1]

    def does_collide(self, rect):
        return self.rect.colliderect(rect)

    def update(self):
        pass
