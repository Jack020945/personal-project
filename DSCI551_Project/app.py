from flask import Flask, render_template, request, flash, redirect, url_for
from sqlalchemy import create_engine, text

app = Flask(__name__)

# Database configuration
DATABASE_URI = 'mysql+pymysql://root:Zhkorl007trjjxy!@localhost/ssense'
engine = create_engine(DATABASE_URI)

app.secret_key = 'secret_key'

@app.route('/query')
@app.route('/')
def home():
    current_page = request.args.get('current_page', 1, type=int)
    per_page_size = request.args.get('per_page_size', 50, type=int)
    filter_sql_statement, filter_url = gen_filter_conditions()

    sql_statement = text(f"SELECT COUNT(*) FROM products {filter_sql_statement}")
    with engine.connect() as conn:
        total_product_size = conn.execute(sql_statement).scalar()

    total_page_size = (total_product_size + per_page_size - 1) // per_page_size

    sql_statement = text(f"SELECT * FROM products {filter_sql_statement} ORDER BY id LIMIT :offset, :limit")
    with engine.connect() as conn:
        data = conn.execute(sql_statement, {'offset': (current_page - 1) * per_page_size, 'limit': per_page_size}).fetchall()

    return render_template('index.html', products=data, total_page_size=total_page_size, current_page=current_page, per_page_size=per_page_size, filter_url=filter_url)

@app.route('/insert', methods=['POST'])
def insert():
    if request.method == "POST":
        brand = request.form['brand']
        description = request.form['description']
        price_usd = request.form['price_usd']
        product_type = request.form['type']
        
        insert_query = text("INSERT INTO products (brand, description, price_usd, type) VALUES (:brand, :description, :price_usd, :type)")
        with engine.connect() as conn:
            conn.execute(insert_query, {'brand': brand, 'description': description, 'price_usd': price_usd, 'type': product_type})
            flash("Data Saved Successfully")
        
        return redirect(url_for('home'))

@app.route('/delete/<string:id_number>', methods=['GET'])
def delete(id_number):
    delete_query = text("DELETE FROM products WHERE id = :id")
    with engine.connect() as conn:
        conn.execute(delete_query, {'id': id_number})
        flash("Product Record Has Been Deleted Successfully")
    
    return redirect(url_for('home'))

@app.route('/update', methods=['POST', 'GET'])
def update():
    if request.method == 'POST':
        id = request.form['id']
        brand = request.form['brand']
        description = request.form['description']
        price_usd = request.form['price_usd']
        product_type = request.form['type']
        update_query = text("UPDATE products SET brand=:brand, description=:description, price_usd=:price_usd, type=:type WHERE id=:id")
        with engine.connect() as conn:
            conn.execute(update_query, {'id': id, 'brand': brand, 'description': description, 'price_usd': price_usd, 'type': product_type})
            flash("Data Updated Successfully")
        return redirect(url_for('home'))


def gen_filter_conditions():
    id = request.args.get('id', None, type=str)
    brand = request.args.get('brand', None, type=str)
    description = request.args.get('description', None, type=str)
    price_usd = request.args.get('price_usd', None, type=int)
    product_type = request.args.get('type', None, type=str)

    filter_sql_statement = ''
    filter_url = ''
    if brand:
        filter_sql_statement = f" brand = '{brand}'"
        filter_url = f'brand={brand}'

    if id:
        filter_sql_statement = f" id = '{id}'"
        filter_url = f'id={id}'

    if description:
        if filter_sql_statement:
            filter_sql_statement += " AND "
        filter_sql_statement += f" description = '{description}'"
        filter_url = f'description={description}'

    if price_usd:
        if filter_sql_statement:
            filter_sql_statement += " AND "

        filter_sql_statement += f" price_usd = {price_usd}"
        filter_url = f'price_usd={price_usd}'

    if product_type:
        if filter_sql_statement:
            filter_sql_statement += " AND "
        filter_sql_statement += f"type = '{product_type}'"
        filter_url = f'type={product_type}'

    if filter_sql_statement:
        filter_sql_statement = ' WHERE ' + filter_sql_statement

    return filter_sql_statement, filter_url


if __name__ == "__main__":
    app.run(host="0.0.0.0",port = 5002,debug=True)
