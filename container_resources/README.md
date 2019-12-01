# Docker image build instructions
Navigate to project root directory and run following command, replacing <imagename> with on of your choosing:
docker build -t <imagename> -f .\container_resources\Dockerfile .
Eg:
docker build -t datathon2019 -f .\container_resources\Dockerfile .

# Docker container creation instructions
Replacing <containername> with on of your choosing and <imagename> with the tag value selected during build or subsequently added to the image run following command:
 docker container create --name datathon2019 -p 3838:3838 --env  PROJECT='datathon2019' --env  PORT='3838' --env OAUTH_API_KEY="i00dr0DT2bIe5jON8qRcBze7jsNcJkmC" --env OAUTH_API_SECRET="QwLGyv-AKiPeO6ZgfDKukGtT9t0BipqW3qobtNAD3w8ib76gRPpx2euDIcF4bv-q" --env OAUTH_API_AUDIENCE="https://datathon2019.au.auth0.com/api/v2/" --env OAUTH_KEY="4LOYSlXx1SS1lNWZQYItvTwaQZLBejkn" --env OAUTH_SECRET="2GNAP9x4uxlbenmj2scBynjoZPa-kzbI-rqCjziX2xLI42FfJXlivp6CGaDUgpAv" --env OAUTH_REDIRECT_URL="http://127.0.0.1:3838" --env OAUTH_PROVIDER="auth0" --env OAUTH_USERINFO_URL="" --env OAUTH_SCOPE="" --env OAUTH_USER="datathon2019"  datathon2019                          

# Docker container run instructions
docker start datathon2019

# Docker container removal instructions
docker rm -f datathon2019