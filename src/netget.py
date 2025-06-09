import requests 
from bs4 import BeautifulShop

def parse_news(str: url):
    try:
        response = requests.get(url)
        response.raise_for_status()
        soup = BeautifulSoup(response.text, "html.parser")

        news_items = soup.find_all("h3", class_="gs-c-promo-heading__title")

        news_list = []

        for item in news_items:
            parent = item.find_parent("a")
            if parent:
                title = item.get_text(strip=True)
                link = parent.get("href")
                if not link.startswith("http"):
                    link = "https://www.bbc.com" + link 
                news_list.append({"title": title, "link": link})


        return news_list
    except requests.RequestException as e:
        print(f"Error: {e}")
        return []
def main():
    url = "https://ww.bbc.com/news"
    news = parse_news(url)
    if news:
        print(f"Find: {len(news)}")
        for i, item in enumerate(news, 1):
            print(f"{i}. {item['title']}")
            print(f"Link: {item['link']}\n\n")

    
if __name__ == "__main__":
    main()
