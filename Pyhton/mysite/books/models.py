from django.db import models
from django.utils import timezone
import datetime


# Create your models here.


class Publisher(models.Model):
    name = models.CharField(max_length=30)
    address = models.CharField(max_length=50)
    city = models.CharField(max_length=60)
    country = models.CharField(max_length=20)
    state_province = models.CharField(max_length=50, blank=True, null=True, default=None)
    website = models.URLField()

    def __str__(self):
        return self.name

    class Meta:
        ordering = ['-name']


class Author(models.Model):
    first_name = models.CharField(max_length=30)
    last_name = models.CharField(max_length=40)
    email = models.EmailField(blank=True)

    def __str__(self):
        return u'%s %s' % (self.first_name, self.last_name)



class BookManager(models.Manager):
    def title_count(self, keyword):
        return self.filter(title__icontains=keyword).count()

    

class Book(models.Model):
    title = models.CharField(max_length=100)
    authors = models.ManyToManyField(Author)
    publisher = models.ForeignKey(Publisher)
    publication_date = models.DateField()
    num_pages = models.IntegerField(blank=True, null=True)
    objects = BookManager()

    def recent_publication(self):
        return self.publication_date >= timezone.now().date()-datetime.timedelta(weeks=8)


    def __str__(self):
        return self.title


1

