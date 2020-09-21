from django.conf.urls import url

from .views import ( 
                    ShopListView, FindMapView
                   )

urlpatterns = [
    url(r'^$', ShopListView.as_view(), name='list'),
    url(r'^find_shop$', FindMapView.as_view(), name='find'),

]

