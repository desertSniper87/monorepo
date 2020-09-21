import sys
import time

from PySide2.QtWidgets import QApplication
from PySide2.QtWidgets import QLabel
from PySide2.QtCore import QTime, QTimer, QCoreApplication

if __name__ == "__main__":
    app = QApplication(sys.argv)
    message = "Alert!"

    due = QTime.currentTime()

    try:
        if len(sys.argv)<2:
            raise ValueError


        hours, minutes = sys.argv[1].split(":")
        if not due.isValid():
            raise ValueError

        due = QTime(int(hours), int(minutes))
        while QTime.currentTime() < due :
            time.sleep()

        label = QLabel("<font color=red size=72>Alarm Clock </font>")
        label.show()

        quit()


    except Exception as e:
        print(e)
        quit()

    sys.exit(app.exec_())

def quit():
    QTimer.singleShot(5000, QCoreApplication.quit)
