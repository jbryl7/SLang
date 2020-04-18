FROM "mozilla/sbt:8u232_1.3.8"
WORKDIR '/app'
COPY build.sbt .
COPY project/build.properties project/build.properties
COPY project/plugins.sbt project/plugins.sbt
RUN sbt reload
COPY . .
CMD ["sbt"]
