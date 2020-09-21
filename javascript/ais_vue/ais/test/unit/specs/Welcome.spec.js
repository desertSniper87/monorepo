import Vue from 'vue'
import Welcome from '@/components/Welcome'

/*
 * The fictional user in our test is Mr. Abdul Baten, age 26. He lives in ______
 * village of _____ upazila of ______ district.
 */
 
describe('Welcome.vue', () => {
  it('should render correct contents', () => {
    const Constructor = Vue.extend(Welcome)
    const vm = new Constructor().$mount()

    /*
     * When Mr Baten First comes to our site, he sees an welcome message in h1 tag.
     */
    expect(vm.$el.querySelector('.welcome h1').textContent)
      .toEqual('Welcome to Agricultural Information System (AIS)')

    /*
     * He also sees a navbar which has the navs of Homepage,
     * 
     */
  })

})
