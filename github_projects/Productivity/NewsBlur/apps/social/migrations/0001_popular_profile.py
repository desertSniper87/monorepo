# encoding: utf-8
import datetime
import random
from south.db import db
from south.v2 import DataMigration
from django.db import models
from django.contrib.auth.models import User
from apps.social.models import MSocialProfile
from mongoengine.queryset import OperationError

class Migration(DataMigration):

    def forwards(self, orm):
        "Write your forwards methods here."
        user = User.objects.create(username='popular', email='popular@newsblur.com')
        user.set_password("%s-%s" % (random.random(), random.random()))
        user.save()
        try:
            profile, _ = MSocialProfile.objects.get_or_create(user_id=user.pk)
            profile.save()
        except OperationError:
            print " ---> Whoops, already have a popular user."

    def backwards(self, orm):
        "Write your backwards methods here."


    models = {
        
    }

    complete_apps = ['social']
