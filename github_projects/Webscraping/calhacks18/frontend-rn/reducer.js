export const USER_SET = '/user/set';

export function reducer(state = { repos: [] }, action) {
  switch (action.type) {
    case USER_SET:
      return { ...state, userId: action.payload };
    default:
      return state;
  }
}

export function setUser(userId) {
  return {
    type: USER_SET,
    payload: userId
  };
}
