from django.conf.urls import url, include
from django.contrib import admin

from django.conf import settings
from django.conf.urls.static import static

from django.apps import apps
from django .views.generic import TemplateView

from .views import hello_world, home_page, about_page,\
        contact_page, login_page, register_page, logout_page
# from search.views import SearchProductView
from carts.views import cart_detail_api_view

products_app_name = apps.get_app_config('products').verbose_name
search_app_name = apps.get_app_config('search').verbose_name
carts_app_name = apps.get_app_config('carts').verbose_name

urlpatterns = [
    url(r'^hello/$', hello_world),
    url(r'^googleb861e71a1435ed1e.html/$', TemplateView.as_view(template_name='googleb861e71a1435ed1e.html')),

    url(r'^$', home_page, name='home'),
    # url(r'^bootstrap/$', TemplateView.as_view(template_name='bootstrap/example.html')),
    url(r'^admin/', admin.site.urls),
    url(r'^about/$', about_page),
    url(r'^contact/$', contact_page, name='contact'),
    url(r'^login/$', login_page, name='login'),
    url(r'^logout/$', logout_page, name='logout'),
    url(r'^register/$', register_page, name='register'),

    url(r'^api/cart/$', cart_detail_api_view, name='api-cart'),

    # url(r'^search/', SearchProductView.as_view(), namespace='search'),
    url(r'^search/', include(('search.urls', search_app_name), namespace='search')),

    url(r'^products/', include(('products.urls', products_app_name), namespace='products')),

    url(r'^carts/', include(('carts.urls', carts_app_name), namespace='carts')),

]
# + static(settings.STATIC_URL, document_root = settings.STATIC_ROOT) + static(settings.MEDIA_URL, document_root = settings.MEDIA_ROOT)
# if settings.DEBUG:
    # urlpatterns = urlpatterns + static(settings.STATIC_URL, document_root = settings.STATIC_ROOT) + static(settings.MEDIA_URL, document_root = settings.MEDIA_ROOT)

if settings.DEBUG:
    urlpatterns = urlpatterns + static(settings.STATIC_URL, document_root=settings.STATIC_ROOT)
    urlpatterns = urlpatterns + static(settings.MEDIA_URL, document_root=settings.MEDIA_ROOT)
