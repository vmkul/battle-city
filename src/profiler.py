import os
import time

MAX_SAMPLE_SIZE = 1000


class Profiler():
    def __init__(self, func):
        self.func = func
        self.exec_durations = []

    def set_func(self, func):
        self.exec_durations.clear()
        self.func = func

    def execute(self, *args):
        if len(self.exec_durations) == MAX_SAMPLE_SIZE:
            self.exec_durations.pop(0)
        start = time.perf_counter()

        res = self.func(*args)
        self.exec_durations.append((time.perf_counter() - start) * 1000)

        return res

    def get_avg_duration(self):
        if len(self.exec_durations) == 0:
            return 0
        return sum(self.exec_durations) / len(self.exec_durations)

    def get_func_name(self):
        return self.func.__name__

    def get_func(self):
        return self.func
