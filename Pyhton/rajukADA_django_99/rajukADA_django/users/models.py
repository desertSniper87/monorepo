from django.contrib.auth.models import AbstractUser
from django.core.urlresolvers import reverse
from django.db import models
from django.utils.encoding import python_2_unicode_compatible
from django.utils.translation import ugettext_lazy as _


@python_2_unicode_compatible
class User(AbstractUser):          #APPLICANT IN USER

    # First Name and Last Name do not cover name patterns
    # around the globe.
    #Django should manage passwords
    # apply_no = models.PositiveSmallIntegerField('Application No.')
    id = models.CharField(_('ID'), primary_key=True, max_length=30)
    name = models.CharField(_('Name of User'), blank=False, max_length=30)
    gender = models.CharField(_('Gender'), blank=False, max_length=1, default='M')
    present_address = models.CharField(_('Present Address'), blank=True, max_length=50)
    permanent_address = models.CharField(_('Present Address'), blank=True, max_length=50)
    father_name = models.CharField(_('Father\'s name of User'), blank=True, max_length=30)
    mother_name = models.CharField(_('Mother\'s name of User'), blank=True, max_length=30)
    phone_number =models.PositiveIntegerField(blank=True)
    email = models.EmailField(blank=False)
    picture_path = models.FilePathField(_('Picture'), blank=True)
    #TODO Make the switch

    def __str__(self):
        return self.username

    def get_absolute_url(self):
        return reverse('users:detail', kwargs={'username': self.username})
