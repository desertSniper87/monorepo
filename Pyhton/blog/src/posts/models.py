from django.db import models

# Create your models here.

class Post(models.Model):

    """Docstring for Post. """

    title = models.CharField(max_length=120)
    content = models.TextField()
    update_time = models.DateTimeField(auto_now=True, auto_now_add=False)
    timestamp = models.DateTimeField(auto_now=False, auto_now_add=True)

    def __str__(self):
        return self.title

        
