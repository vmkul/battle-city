import pygame as pg


class PathSymbol(pg.sprite.Sprite):
    def __init__(self, row, col):
        pg.sprite.Sprite.__init__(self)
        self.image = pg.Surface((32, 32))
        pg.draw.circle(self.image, (0, 255, 0), (16, 16), 4)
        self.rect = self.image.get_rect()
        self.rect.topleft = col * 32, row * 32
