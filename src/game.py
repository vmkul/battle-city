import os
import random
import pygame as pg
from wall import *
from tank import *
from bullet import *
from util import *

random.seed()

class Game:
    def __init__(self):
        pg.init()
        screen = pg.display.set_mode((480, 426))
        self.screen = screen
        pg.display.set_caption("Battle City")

        background = pg.Surface(screen.get_size())
        background = background.convert()
        background.fill((128, 128, 128))
        self.background = background

        self.game_map = pg.Surface((416, 416))
        self.game_map = self.game_map.convert()
        self.game_map.fill((0, 0, 0))
        self.game_map_rect = self.game_map.get_rect().move(MAP_COORDINATES[0], MAP_COORDINATES[1])

        background.blit(self.game_map, MAP_COORDINATES)
        screen.blit(background, (0, 0))

        self.bullet_sprites = pg.sprite.RenderPlain()
        self.wall_sprites = pg.sprite.RenderPlain()
                
        self.player_tank = Tank(0, 0, self)
        self.tank_sprites = pg.sprite.RenderPlain((self.player_tank, AITank(100, 300, self), AITank(300, 100, self), AITank(222, 123, self)))

        for i in range(13):
            for j in range(13):
                if random.random() < 0.2:
                    self.wall_sprites.add(Wall(i, j))

    def main_loop(self):
        last_shot_time = -1000
        pg.display.flip()
        clock = pg.time.Clock()

        going = True
        pressed_directions = []

        while going:
            clock.tick(60)

            if len(pressed_directions) > 0:
                last_pressed = pressed_directions[-1]

                if last_pressed == pg.K_UP:
                    self.player_tank.move_up()
                elif last_pressed == pg.K_DOWN:
                    self.player_tank.move_down()
                elif last_pressed == pg.K_RIGHT:
                    self.player_tank.move_right()
                elif last_pressed == pg.K_LEFT:
                    self.player_tank.move_left()

            for event in pg.event.get():
                if event.type == pg.QUIT:
                    going = False

                elif event.type == pg.KEYDOWN:
                    if event.key == pg.K_UP or event.key == pg.K_DOWN or event.key == pg.K_RIGHT or event.key == pg.K_LEFT:
                        pressed_directions.append(event.key)
                    if event.key == pg.K_SPACE:
                        current = pg.time.get_ticks()
                        if (current - last_shot_time) >= 1000:
                            last_shot_time = current
                            self.player_tank.shoot()

                elif event.type == pg.KEYUP:
                    if event.key in pressed_directions:
                        pressed_directions.remove(event.key)
                        if len(pressed_directions) == 0:
                            self.player_tank.stop()


            self.tank_sprites.update()
            self.wall_sprites.update()
            self.bullet_sprites.update()

            self.screen.blit(self.background, (0, 0))
            self.screen.get_rect().inflate(100, 100)

            self.wall_sprites.draw(self.screen)
            self.bullet_sprites.draw(self.screen)
            self.tank_sprites.draw(self.screen)
            pg.display.flip()

        pg.quit()

if __name__ == "__main__":
    game = Game()
    game.main_loop()
