from django.contrib.postgres.fields import JSONField
from django.core.exceptions import ValidationError
from django.db import models
from django.contrib.auth.models import User


class RiskType(models.Model):
    name = models.CharField(max_length=255, unique=True)
    created_by = models.ForeignKey(
        User, on_delete=models.SET_NULL,
        null=True
    )
    attribute_collection = JSONField(blank=True, null=True)


class Risk(models.Model):
    user = models.ForeignKey(User, on_delete=models.CASCADE)
    name = models.CharField(max_length=255)
    date_created = models.DateTimeField("Date Created", auto_now_add=True)
    risk_type = models.ForeignKey(
        RiskType, on_delete=models.SET_NULL,
        null=True
    )
    risk_attributes = JSONField(blank=True, null=True)

    def save(self, *args, **kwargs):
        try:
            specific_risk_attributes = self.risk_type.attribute_collection
            for i in self.risk_attributes:
                if i not in specific_risk_attributes:
                    raise ValidationError("No attribute does not match risk_type")
        except TypeError as t:
            pass

        return super(Risk, self).save(*args, **kwargs)









