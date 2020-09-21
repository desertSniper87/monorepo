from django.conf import settings
from django.conf.urls import url


from . import views

urlpatterns = [

    url('^ajax/$', views.handle_ajax, name='ajax'),
    url('^', views.home, name="home"),
    # user profiles

]
