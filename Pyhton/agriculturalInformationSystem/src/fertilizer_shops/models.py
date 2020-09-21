#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Author            : desertsniper87 <torshobuet@gmail.com>
# Date              : 05.03.2018
# Last Modified Date: 05.03.2018
from django.db import models

class FertilizerShopManager(models.Manager):
    # def get_queryset(self):
        # return ProductQuerySet(self.model, using=self._db)

    def all(self):
        return self.get_queryset()

    # def features(self):
        # return self.get_queryset().filter(featured=True)

    # def get_by_id(self, id):
        # qs = self.get_queryset().filter(id=id)
        # if qs.count()==1:
            # return qs.first()
        # return None

class Fertilizer_shop(models.Model):    #Product_category
    id            = models.AutoField(primary_key = True)
    name          = models.CharField(max_length  = 30, blank = True)
    village       = models.CharField(max_length  = 30, blank = True)
    union_council = models.CharField(max_length  = 30, blank = True)
    upazila       = models.CharField(max_length  = 30, blank = True)
    district      = models.CharField(max_length  = 30, blank = True)
    division      = models.CharField(max_length  = 30, blank = True)

    objects = FertilizerShopManager()

    # def get_absolute_url(self):
        # # return "/products/{slug}".format(slug=self.slug)
        # return reverse("products:detail", kwargs={"slug": self.slug})
        # # return reverse("products:list", kwargs={"slug": self.slug})

    def __str__(self):
        return self.name


