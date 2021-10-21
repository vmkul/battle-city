import signal
from datetime import datetime


class Logger:
    def __init__(self):
        filename = datetime.now().strftime("%d-%b-%Y_%H:%M:%S.%f") + ".csv"
        self.file = open(f"./logs/{filename}", "w")
        signal.signal(signal.SIGINT, self.close_handler)

    def log_result(self, status, time, score, algorithm):
        self.file.write(f"{status}, {time}, {score}, {algorithm}\n")

    def close_handler(self, signum, frame):
        self.file.close()
        exit(0)


class LoggerMock:
    def log_result(self, status, time, score, algorithm):
        pass
