from ghost import Ghost, Session

ghost = Ghost()
USERAGENT = "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:45.0) Gecko/20100101 Firefox/45.0"

with ghost.start():
    session = Session(ghost, download_images=False, display=True, user_agent=USERAGENT, viewport_size=(800, 600))
    page, rs = session.open("https://m.facebook.com/login.php", timeout=120)
    assert page.http_status == 200

    session.evaluate("""
    document.querySelector('input[name="email"]').value = 'torshobuet@gmail.com'
    document.querySelector('input[name="pass"]').value = 'wikipedia150101facebook';
    """)

    session.evaluate("""document.querySelector('input[name="login"]').click();""",
                 expect_loading=True)

    """
    import codecs

    with codecs.open('fb.html', encoding='utf-8', mode='w') as f:
       f.write(session.content)
    """

    # session.save_cookies('fbookie')
    session.capture_to(path='fbookie.png')

    # gracefully clean off to avoid errors
    session.webview.setHtml('')
    session.exit()
