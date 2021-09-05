import os
import random
import pygame as pg
from wall import *
from tank import *
from bullet import *
from base import *
from util import *

random.seed()
RESTRICTED_TILES = [(12, 5), (12, 6), (12, 7), (11, 5), (11, 6), (11, 7), (10, 6)]
SPAWN_ENEMY_EVENT = pg.USEREVENT

class Game:
    def __init__(self):
        pg.init()
        screen = pg.display.set_mode((480, 426), flags=pg.SCALED)
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
        self.base_sprite = pg.sprite.RenderPlain(Base())
        self.wall_sprites = pg.sprite.RenderPlain((
            Wall(12, 5),
            Wall(12, 7),
            Wall(11, 5),
            Wall(11, 6),
            Wall(11, 7),
        ))

        self.tile_map = 13 * [13 * [0]]
        self.map_coords = []

        for i in range(13):
            for j in range(13):
                if (i, j) not in RESTRICTED_TILES:

                    self.map_coords.append((i, j))


        for i in range(40):
            coord = random.choice(self.map_coords)
            self.map_coords.remove(coord)
            self.wall_sprites.add(Wall(coord[0], coord[1]))


        self.player_tank = Tank(10, 6, self)
        self.tank_sprites = pg.sprite.RenderPlain((self.player_tank))

        self.enemy_count = 4
        for i in range(self.enemy_count):
            self.spawn_enemy()

    def spawn_enemy(self):
        rows = [n for n in range(13)]
        cols = [n for n in range(13)]

        random.shuffle(rows)
        random.shuffle(cols)

        for i in rows:
            for j in cols:
               if (i, j) in RESTRICTED_TILES:
                   continue
               test_rect = pg.Rect(j * 32 + MAP_COORDINATES[0], i * 32 + MAP_COORDINATES[1], 32, 32)

               collides = False

                #TODO: Create game class methods for checking collisions

               for wall in self.wall_sprites:
                   if wall.rect.contains(test_rect):
                       collides = True

               for tank in self.tank_sprites:
                   if tank.rect.colliderect(test_rect):
                       collides = True

               if collides:
                   continue                
               return self.tank_sprites.add(AITank(i, j, self))
                
    def main_loop(self):
        last_shot_time = -1000
        pg.display.flip()
        clock = pg.time.Clock()
        spawn_pending = False

        going = True
        pressed_directions = []

        while going:
            clock.tick(60)

            if len(self.tank_sprites) < 4 and not spawn_pending:
                pg.time.set_timer(SPAWN_ENEMY_EVENT, 2000)
                spawn_pending = True

            if len(self.base_sprite) == 0:
                exit()

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
                
                elif event.type == SPAWN_ENEMY_EVENT:
                    pg.time.set_timer(SPAWN_ENEMY_EVENT, 0)
                    self.spawn_enemy()
                    spawn_pending = False

            self.tank_sprites.update()
            self.wall_sprites.update()
            self.bullet_sprites.update()
            self.base_sprite.update()

            self.screen.blit(self.background, (0, 0))

            self.wall_sprites.draw(self.screen)
            self.bullet_sprites.draw(self.screen)
            self.tank_sprites.draw(self.screen)
            self.base_sprite.draw(self.screen)
            pg.display.flip()

        pg.quit()

if __name__ == "__main__":
    game = Game()
    game.main_loop()
