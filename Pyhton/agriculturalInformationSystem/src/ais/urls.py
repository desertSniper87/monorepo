"""ais URL Configuration

The `urlpatterns` list routes URLs to views. For more information please see:
    https://docs.djangoproject.com/en/2.0/topics/http/urls/
Examples:
Function views
    1. Add an import:  from my_app import views
    2. Add a URL to urlpatterns:  path('', views.home, name='home')
Class-based views
    1. Add an import:  from other_app.views import Home
    2. Add a URL to urlpatterns:  path('', Home.as_view(), name='home')
Including another URLconf
    1. Import the include() function: from django.urls import include, path
    2. Add a URL to urlpatterns:  path('blog/', include('blog.urls'))
"""
from django.contrib import admin
from django.conf.urls import url, include
from .views import home_page, UserViewSet, GroupViewSet
from fertilizer_shops.views import ShopListAPIViewSet
from django.apps import apps

from rest_framework import routers

fertilizer_shop_app_name = apps.get_app_config('fertilizer_shops').verbose_name

router = routers.DefaultRouter()
router.register(r'users', UserViewSet)
router.register(r'groups', GroupViewSet)

# router.register(r'fertilizer', ShopListAPIView)
router.register(r'fertilizer', ShopListAPIViewSet)

urlpatterns = [
    url(r'^api/', include(router.urls)),
    url(r'^api-auth/', include('rest_framework.urls', namespace='rest_framework')),

    url(r'^$', home_page, name='home'),
    url(r'^fertilizer/', include(('fertilizer_shops.urls', fertilizer_shop_app_name), namespace='fertilizer_shops')),
    url(r'^admin/', admin.site.urls),
]


