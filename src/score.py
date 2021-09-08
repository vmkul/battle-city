import pygame as pg
from util import *


counter_font = pg.font.Font(
    data_dir + "/fonts/ARCADECLASSIC.TTF", 26)


class EnemyCounter(pg.sprite.Sprite):
    def __init__(self, count):
        pg.sprite.Sprite.__init__(self)
        self.tank_sprite, self.tank_rect = load_image(
            "sprites/tank_symbol.png", -1)
        self.tank_sprite = pg.transform.scale(self.tank_sprite, (48, 27))

        self.image = pg.Surface((48, 66))
        self.rect = self.image.get_rect()
        self.rect.bottomright = 480, 200

        self.count = count

    def set_count(self, new_val):
        self.count = new_val

    def update(self):
        self.image.fill((128, 128, 128))
        self.image.blit(self.tank_sprite, (0, 0))

        text = counter_font.render(f"{self.count}X", 1, (0, 0, 0))
        width = (48 - text.get_rect().width) / 2

        self.image.blit(text, (width, 33))
