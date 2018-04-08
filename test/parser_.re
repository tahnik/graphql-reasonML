let test1 = "query GetUserAndPosts {
  getUser(id: \"hello\") {
    id
    name
    posts(first: \"word\") {
      id
      title
      content
    }
  }
}";

let test2 = "query GetCityEvents {
  getCity(newLine: \"id-for-san-francisco\") {
    id
    name
    events {
      edges {
        node {
          id
          name
          date
          sport {
            id
            name
          }
        }
      }
    }
  }
}";

let test3 = "query FootballEventsInSeattle {
  viewer {
    allEvents(where: {
      date: {
        gt: \"2017\"
      },
      city: {
        name: {
          eq: \"Seattle\"
        }
      },
      sport: {
        name: {
          eq: \"Football\"
        }
      }
    }) {
      edges {
        node {
          id
          name
          sport {
            id
            name
          }
        }
      }
      aggregations {
        count
      }
    }
  }
}";

for (index in 0 to 50000) {
  Parser.parse(test3);
};