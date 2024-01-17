import sys
import spotipy.util as util

def main():
    username, client_id, client_secret, scope, redirect_uri = sys.argv[1:6]
    token = util.prompt_for_user_token(username, scope,client_id=client_id,client_secret=client_secret,redirect_uri=redirect_uri) 
    print(token,end='')

if __name__ == "__main__":
    main()
