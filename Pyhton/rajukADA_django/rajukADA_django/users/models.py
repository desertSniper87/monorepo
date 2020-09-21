from django.contrib.auth.models import AbstractUser
from django.core.urlresolvers import reverse
from django.db import models
from django.utils.encoding import python_2_unicode_compatible
from django.utils.translation import ugettext_lazy as _
from django.core.validators import RegexValidator

@python_2_unicode_compatible
class User(AbstractUser):          #APPLICANT IN USER

    # First Name and Last Name do not cover name patterns
    # around the globe.
    #Django should manage passwords
    # apply_no = models.PositiveSmallIntegerField('Application No.')
    # id = models.CharField(_('ID'), primary_key=True, max_length=254)
    # user_id = models.PositiveIntegerField(primary_key=True)
    phone_regex = RegexValidator(regex=r'^\+?1?\d{9,15}$', message="Phone number must be entered in the format: '+999999999'. Up to 15 digits allowed.")

    name = models.CharField(_('Name of User'), blank=False, max_length=254)
    gender = models.CharField(_('Gender'), blank=False, max_length=252, default='M')
    present_address = models.CharField(_('Present Address'), blank=True, max_length=255)
    permanent_address = models.CharField(_('Present Address'), blank=True, max_length=255)
    father_name = models.CharField(_('Father\'s name of User'), blank=True, max_length=254)
    mother_name = models.CharField(_('Mother\'s name of User'), blank=True, max_length=254)
    # phone_number =models.PositiveIntegerField(blank=True, null=True)
    phone_number = models.CharField(validators=[phone_regex], blank=True, max_length=253)
    # email = models.EmailField(blank=False)
    #TODO how to upload a picture
    # picture = models.FileField(_('Picture'), blank=True, upload_to='picture/')
    #TODO Make the switch : DONE

    def __str__(self):
        return self.username

    def get_absolute_url(self):
        return reverse('users:detail', kwargs={'username': self.username})


