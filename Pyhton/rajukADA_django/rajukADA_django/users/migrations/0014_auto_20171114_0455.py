# -*- coding: utf-8 -*-
# Generated by Django 1.10.6 on 2017-11-13 22:55
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('users', '0013_auto_20171114_0453'),
    ]

    operations = [
        migrations.AlterField(
            model_name='user',
            name='username',
            field=models.CharField(max_length=255, verbose_name='Username'),
        ),
    ]