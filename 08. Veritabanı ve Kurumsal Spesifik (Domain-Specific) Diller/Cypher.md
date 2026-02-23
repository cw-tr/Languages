# Cypher (Neo4j)

## Özet
Cypher; 2011 yılında Neo4j şirketi tarafından icat edilen ve günümüzde "openCypher" projesi adı altında Açık Kaynak standardı haline gelen, Geleneksel İlişkisel (SQL / Tablo) veritabanlarındaki o iğrenç (Yüzlerce JOIN ile atılan) tablo birleştirme işlemlerini tarihe gömen, tamamen çizimlere, noktalara (Node) ve o noktaları bağlayan ok/çizgi yönlerine (Relationship) odaklanan bir **Çizge Veritabanı (Graph Database) Sorgu Dilidir**. 

## Nedir ve Ne İşe Yarar?
Eski veritabanlarında (SQL vb. Excel yapılarında) veriler sütunlar ve sıralar halindeydi. Eğer "Ali'nin Arkadaşının Satın Aldığı Ürünleri Tavsiye Eden Adam Kimdir?" diye (Zincirleme / Çoklu İlşki) bir Soru Sormak istiyorsanız SQL kodunda en az 4 tane `INNER JOIN` tablosu bağlamanız gerekirdi. Bu işlem SQL'i Kilitler, Veritabanını yorar (N+1 Sorgu eziyeti) ve Dakikalar(Milyonuncu satırda saatler) sürerdi.

Cypher (Neo4j) Dedi ki: Verileri Tablo Tutmayalım! **Bütün Verileri Evrendeki Noktalar (Düğümler / Node) Ve onları bağlayan Yönlü Oklar (Edge/Relationship) olarak** tutalım! Mimarinin Adına Çizge/Ağ (Graph) denildi. Böylece bir Kökten Yola Çıkıp "Oka Takip Ederek" Milyonlarca veriyi SQL'den 1000 Kat daha Işık hızında tarayan Muazzam mimari doğdu.

**Ne İşe Yarar?**
* **Sosyal Medya Asistalığı (Öneri Motorları):** Netflix'teki "Bunu İzleyenler Bunları da İzledi", "Ali Tanıyor Olabilirsin (Ortak 19 Arkadaşınız var)" tarzı Algoritmaların SQL tablolarıyla yapılması İntihardır; Graph Database ve Cypher sorgularıyla yapılması Ekran Kartının Göz Kırpması hızındadır (0.1ms).
* **Siber Güvenlik ve Sahtekarlık (Fraud) Tespiti:** Bir kredi kartı hesabından "Kara Para aklama Çemberini (Mule Ring)" bulmak, Hangi hesapların Kendi arasında 3. elden paslaşıp Ortak hesaba döndüğünü Taradığınız Muazzam İstihbarat Kütüphaneleridir.

## Dilin Mantığı ve Kod Yapısı
Sözdizimi (Syntax), Beyin Yakıcı ölçüde **Görsel ASCII Sanatı (ASCII-Art)** gibidir!
Adamlar Kodu yazarken Tablolar/Joınleri kullanmamiş. Dümdüz Simgelerle "Top (Düğüm)" Ve Ok Çizimleriyle sorgu Atılmasını sağlamıştır.

Mantığı:
1. `( )` (Yuvarlak Parantez): **Bir Düğümü (Node)** temsil eder. Yani Bir "Kişi" veya "Ürün" (Yuvarlak top gibidir).
2. `[ ]` (Köşeli Parantez): **İlişkiyi (Relationship)** temsil eder. Yani "Sevmek", "Satın Almak".
3. `->` (Tire Ok): **Yönü (Direction)** belirtir. Yani A->B 'yi Sever (B, A'yı Sevmiyor olabilir Yön mühimdir).

### Örnek Bir Cypher (Graph) Sorgusu: Matrix'teki Gibi Sosyal Ağ Analizi!
SQL'de Yazması(JOIN) 4-5 Sayfa sürecek olan Bir Öneri (Tavsiye Sistemi/Reccomendation) kodunun Cypher ile Karatahtaya Tebeşir çizer gibi Yazılması:

```cypher
/* CYPHER SORGUSU (SQL'DIR LAKIN OKLAR VE ASCIII YUVARLAKLARI GUCU) */

// AMAÇ: "Keanu Reeves" in rol aldigi Filmleri bul. 
//  SONRA O Ofilmlerde Rol alan "DIGER AKTORLERI" bul 
// Ve Ekrana Sadece O Yeni Aktorlerin isimlerini yolla!  (Ortak Projelerden Kisi Kesfi).

// 1. ESLEŞME(MATCH) = SQL deki 'SELECT' Karsiligi (Deseni Aciyoruz)

MATCH (keanu:Person {name: "Keanu Reeves"}) -[:ACTED_IN]-> (film:Movie) <-[:ACTED_IN]- (digerAktor:Person)

// Yukaridaki ASCII Ciziminin Okunusu: 
// Oyle bir (Keanu) yuvarlağı Bul ki Röl almis (ACTED_IN oku tasiyor) bir (film) yuvarlağına!
// Sonra Ayni Filmin Icine Dogru Girmis Baska Bir <-[:ACTED_IN]- (digerAktor) yuvarlalgı bul.


// 2. DÖNÜŞ (RETURN): Sql'deki Ciktilama(Sutun Secimi) Krsiligi
RETURN digerAktor.name

// Siralama(Order): SQL'deki gibi
ORDER BY digerAktor.name ASC


/* VEYAA! Sahtekarlik Çete Bulma (Dolancdirici Zinciri) Aglari : */
/* 5 Uzaklikta(Derecede) Dolaylı Yoldan Paranin Döndüğü hesapları saptama: */
MATCH (suclu:Hesap {id:999}) -[:PARA_GONDERDI*1..5]-> (araciKurbnlar) -[:SON_HESAP_KUTUSU]-> (karteliBasibul)
RETURN karteliBasibul
```
Bu sistem; İlişki Odaklı verilerde (Ağ/Network topolojilerinde) Dünyanın en iyi ve En zeki Makinesi konumundadır. Matematikteki Graf (Graph) ve Node(Düğüm) teorisini Alfabeye dökmüş halidir.

## Kimler Kullanır?
* Evrendeki bütün **Tavsiye Motoru (Reccomendation Engine) Geliştiricileri** ve Veri Bilimcileri. 
* Amazon (AWS Neptune) gibi Büyük Veri Platformlarında Ağ topolojisi ve Kredi Kartı dolandırıcılıkalrını İzleyen İstihbarat Mühendisleri ve Finans Mimarları.
* Sosyal Ağ (Arkadaşın Arkadaşını Önerme) sistemini En az kodla / En hızlı saniyede kurmak İsteyen Yazılımcılar. Cypher (Neo4j), Relational Database (SQL)'nin ezemediği tek Krallıktır.
