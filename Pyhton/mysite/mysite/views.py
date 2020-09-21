from django.http import HttpResponse

from django.template import Template, Context

from django.template.loader import get_template

from django.shortcuts import render

import datetime

from mysite.forms import ContactForm
from django.http import HttpResponseRedirect
from django.core.mail import send_mail


def hello(request):
    return HttpResponse("Hello World")


# def current_datetime(request):
#     now = datetime.datetime.now()
#     html =  "<html><body>It is now %s.</body></html>" % now
#     return HttpResponse(html)

# def current_datetime(request):
#     now = datetime.datetime.now()
#     t = Template("<html><body>It is now {{current_date}}</body></html>")
#     html = t.render(Context({'current_date': now}))
#     return HttpResponse(html)


def current_datetime(request):
    now = datetime.datetime.now()
    t = get_template('current_datetime.html')
    html = t.render(Context({'current_date': now}))
    return HttpResponse(html)


def my_homepage_view(request):
    t = get_template('mypage.html')
    html = t.render(Context({'current_section': "Home Page"}))
    return HttpResponse(html)


def contact(request):
    if request.method == 'POST':
        form = ContactForm(request.POST)
        if form.is_valid():
            cd = form.cleaned_data
            send_mail(
                cd['subject'],
                cd['message'],
                cd.get('email', 'noreply@example.com'),
                ['siteowner@example.com']
                # cd.get('email',
                # 'noreply@example.com'['mailto:noreply%40example.com'],
                # [['siteowner@example.com']('mailto:siteowner%40example.com')],
                # )
            )
            return HttpResponseRedirect('/contact/thanks/')

    else:
        form = ContactForm(
            initial={'subject': 'What is this piece of crap!'}
        )

    return render(request, 'contact_form.html', {'form': form})

    # def my_homepage_view(request, imagePath=None):
    #     html = "<html>" \
    #            "    <body>" \
    #            "        It is homepage"
    #
    #     return HttpResponse(html)
